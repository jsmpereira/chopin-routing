;;; -*- Mode: Lisp -*-

(in-package :chopin-routing)

(defparameter *this-node* nil)
(defparameter *socket* nil)

;; stats
(defparameter *messages-received* 0)

(defparameter *config* nil)

(defparameter *msg-seq-num* 0) ; wrap-around is 65535
(defparameter *pkt-seq-num* 0) ; same here

(defparameter *base-station-p* nil)

(defparameter *msg-types* '(:base-station-beacon 1 :node-beacon 2))
(defparameter *tlv-types* '(:relay 1 :path 2))

(defparameter *out-buffer* (sb-concurrency:make-queue))

(defparameter *duplicate-set* (make-hash-table :test 'equal))

(defclass duplicate-tuple ()
  ((orig-addr :initarg :orig-addr :accessor orig-addr)
   (msg-type :initarg :msg-type :accessor msg-type)
   (seq-num :initarg :seq-num :accessor seq-num)
   (exp-time :initarg :exp-time :accessor exp-time)))

(defun icmp (target)
  (sb-ext:run-program "/sbin/ping" `("-c 1" ,target) :output *standard-output*))

;;; Utils

;; As per RFC 5444 there are some 4 bit fields. Since they occur in pairs, we encode
;; them in a unsigned-byte 8.

(defun merge-4bit-fields (a b)
  (logior (dpb a (byte 4 4) 0)
          (dpb b (byte 4 0) 0)))

(defun extract-4bit-fields (v)
  (values (ldb (byte 4 4) v)
          (ldb (byte 4 0) v)))

(defun version+pkt-flags (pkt-header)
  (merge-4bit-fields (version pkt-header) (pkt-flags pkt-header)))

(defun (setf version+pkt-flags) (value pkt-header)
  (multiple-value-bind (version flags) (extract-4bit-fields value)
    (setf (version pkt-header) version
	  (pkt-flags pkt-header) flags)))

(defun make-pkt-header ()
  (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*)))

(defun make-msg-header ()
  (make-instance 'msg-header :msg-type (getf *msg-types* :base-station-beacon)))

(defun make-tlv (value)
  (make-instance 'tlv :tlv-type (getf *tlv-types* :relay) :value (usocket:host-byte-order value)))

;;; Message Building

(defun build-packet (pkt-header msg-header tlv)
  (userial:with-buffer (userial:make-buffer)
    (serialize-msg-header msg-header)
    (serialize-tlv tlv)
    ;; msg-size is size of message including msg-header, that is msg-header+tlv
    (setf (msg-size msg-header) (userial:buffer-length))
    (make-instance 'packet :pkt-header pkt-header :message (make-instance 'message :msg-header msg-header :tlv-block (make-instance 'tlv-block :tlv tlv)))))

(defun build-message ()
  (if *base-station-p*
      (build-message)
      (build-message :msg-type (getf *msg-types* :node-beacon) :tlv-type (getf *tlv-types* :path))))

(defun generate-message (&key pkt-header msg-header tlv (msg-type (getf *msg-types* :base-station-beacon)) (tlv-type (getf *tlv-types* :relay)) (tlv-value (usocket:host-byte-order (config-host-address *config*))))
  (let* ((pkt-header (or pkt-header (make-instance 'pkt-header)))
	 (msg-header (or msg-header (make-instance 'msg-header :msg-type msg-type :msg-seq-num (incf *msg-seq-num*))))
	 (tlv (or tlv (make-instance 'tlv :tlv-type tlv-type :value tlv-value)))
	 (packet (build-packet pkt-header msg-header tlv)))
    (sb-concurrency:enqueue packet *out-buffer*)
    packet))

(defun message-hash (field1 field2)
  "Hashing for duplicate set"
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~a~a" field1 field2)))))

;;; Processing

(defun check-duplicate-set (msg-type orig-addr)
  (gethash (message-hash msg-type orig-addr) *duplicate-set*))

(defun update-link-set (msg-orig-addr)
  (let* ((local-addr (config-host-address *config*))
	 (ref-interval (config-refresh-interval *config*))
	 (neighb-holding (config-neighb-hold-time *config*))
	 (l-time (dt:second+ (dt:now) (* neighb-holding ref-interval)))
	 (ls-hash (message-hash local-addr msg-orig-addr)))
    (unless (gethash ls-hash *link-set*) ;; stopped here. Validity time from OSLR makes sense. Need to update time when entry already exists
      (setf (gethash ls-hash *link-set*) (make-instance 'link-tuple :local-addr local-addr :neighbor-addr (usocket:hbo-to-dotted-quad msg-orig-addr) :l-time l-time)))))

(defun process-message (pkt-header msg-header tlv)
  (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
    (update-link-set orig-addr)
    (incf *messages-received*)
    ;; add message to duplicate set
    (let* ((timestamp (dt:now))
	   (dtuple (make-instance 'duplicate-tuple :orig-addr orig-addr :msg-type msg-type :seq-num seq-num :exp-time (dt:second+ timestamp (config-dup-hold-time *config*)))))
      (setf (gethash (message-hash msg-type orig-addr) *duplicate-set*) dtuple)
      ;; update message and enqueue
      (incf hop-count)
      (decf hop-limit)
      ;(generate-message :pkt-header pkt-header :msg-header msg-header :tlv tlv)
      (with-accessors ((tlv-type tlv-type) (value value)) tlv
	(format nil "~A DUP --> exp-time: ~A | hop-count: ~A msg-type: ~A orig-addr: ~A seq-num: ~A content-type: ~A content: ~A buff:  ~A" timestamp (slot-value dtuple 'exp-time) hop-count msg-type (usocket:hbo-to-dotted-quad orig-addr) seq-num tlv-type (usocket:hbo-to-dotted-quad value) (usocket:hbo-to-dotted-quad (msg-orig-addr msg-header)))))))

(defun retrieve-message (buffer)
  "Unserialize the message and check if it should be processed."
  (userial:with-buffer buffer
    (userial:buffer-rewind)
    (let ((pkt-header (unserialize-pkt-header (make-instance 'pkt-header)))
	  (msg-header (unserialize-msg-header (make-instance 'msg-header)))
	  (tlv (unserialize-tlv (make-instance 'tlv))))
      (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) msg-header
	(cond
	  ((= hop-limit 0) nil) ; discard
	  ((= hop-count 255) nil) ; discard
	  ((equal (usocket:hbo-to-dotted-quad orig-addr) (config-host-address *config*)) nil) ; discard
	  ((check-duplicate-set orig-addr seq-num) (format nil "~A" (dt:now))) ; discard
	  ((not (member msg-type *msg-types*)) nil) ;discard

	  (t (process-message pkt-header msg-header tlv)))))))

;; timer / event scheduling

(defun check-duplicate-holding ()
  "Remove *duplicate-set* entries with expired timestamp."
  (loop for key being the hash-keys in *duplicate-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'exp-time))
	do (remhash key *duplicate-set*)))

(defun check-link-set-validity ()
  (loop for key being the hash-keys in *link-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'l-time))
	do (remhash key *link-set*)))

(defun out-buffer-get ()
  (let ((buf (userial:make-buffer)))
    (userial:with-buffer buf
      (let ((packet (sb-concurrency:dequeue *out-buffer*)))
	(when packet
	  (serialize-packet packet))))))

(defun start-timers ()
  "Setup and start timers."
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-duplicate-holding :thread t) 10 :repeat-interval (* 3 (config-refresh-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-link-set-validity :thread t) 60 :repeat-interval (* 3 (config-timer-repeat-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'generate-message :thread t) 5 :repeat-interval (config-refresh-interval *config*))
  (sb-ext:schedule-timer (sb-ext:make-timer #'screen :thread t) 5 :repeat-interval (config-refresh-interval *config*)))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer)))

(defun jitter (time)
  "Add some noise to time interval."
  (dt:second+ time (- (random (float *max-jitter*)))))


;; debug 

(defun print-hash (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
	collect (format nil "K:~A V:~A~%" k v)))

(defun screen ()
  (with-open-file (s (merge-pathnames "screen" (user-homedir-pathname)) :direction :output :if-exists :supersede)
    (format s "----- Routing Table -----~%
~A~%
----- Duplicate Set ----~%
~A~%
------ Link Set -----~%
~A~%" *routing-table* (print-hash *duplicate-set*) (print-hash *link-set*))))


;; util

(defun load-config (&optional (path "quicklisp/local-projects/chopin-routing/.config"))
  (with-open-file (in (merge-pathnames path (user-homedir-pathname)) :direction :input)
    (let ((conf (read in)))
      (setf *config* (apply #'make-config conf))))
  (setf *max-jitter* (/ (config-refresh-interval *config*) 4)))
