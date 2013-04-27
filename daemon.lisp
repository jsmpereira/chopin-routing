;;; -*- Mode: Lisp -*-

(in-package :chopin-routing)

(defparameter *this-node* nil)
(defparameter *socket* nil)

;; stats
(defparameter *messages-received* 0)

(defparameter *config* nil)
(defparameter *max-jitter* nil) ; (/ refresh-interval 4)

(defparameter *msg-seq-num* 0) ; wrap-around is 65535
(defparameter *pkt-seq-num* 0) ; same here

(defparameter *base-station-p* nil)

(defparameter *msg-types* '(:base-station-beacon 1 :node-beacon 2))
(defparameter *tlv-types* '(:relay 1 :path 2))

(defparameter *out-buffer* (sb-concurrency:make-queue))

(defparameter *duplicate-set* (make-hash-table :test 'equal))
(defparameter *link-set* (make-hash-table :test 'equal))
(defparameter *routing-table* (make-hash-table :test 'equal))

(defun icmp (target)
  (sb-ext:run-program "/sbin/ping" `("-c 1" ,target) :output *standard-output*))

;; Object factories

(defun make-pkt-header ()
  (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*)))

(defun make-msg-header (&key (msg-type :base-station-beacon))
  (make-instance 'msg-header :msg-type (getf *msg-types* msg-type) :msg-seq-num (incf *msg-seq-num*)))

(defun make-tlv (value &key (tlv-type :relay))
  (make-instance 'tlv :tlv-type (getf *tlv-types* tlv-type) :value (usocket:host-byte-order value)))

(defun make-tlv-block (tlvs)
  (let ((buff (userial:make-buffer)))
    (userial:with-buffer buff
      (dolist (entry tlvs)
	(serialize-tlv entry)))
    ;; tlvs-length is number of octets of tlvs
    (make-instance 'tlv-block :tlvs-length (length buff) :tlv tlvs)))

(defun make-message (&key msg-header tlv-block)
  (make-instance 'message :msg-header msg-header :tlv-block tlv-block))

(defun make-packet (&key (msg-header (make-msg-header)) tlv-block)
  (make-instance 'packet :pkt-header (make-pkt-header)
		 :message (make-message :msg-header msg-header :tlv-block tlv-block)))

;;; Message Building

(defun build-tlvs (tlv-values &key (tlv-type :relay))
  (loop for value in tlv-values
	collect (make-tlv value :tlv-type tlv-type)))

(defun build-packet (msg-header tlv-block)
  (userial:with-buffer (userial:make-buffer)
    (serialize-msg-header msg-header)
    (serialize-tlv-block tlv-block)
    ;; msg-size is size of message including msg-header, that is msg-header+tlv-block
    (setf (msg-size msg-header) (userial:buffer-length))
    (make-packet :msg-header msg-header :tlv-block tlv-block)))

(defun build-message ()
  (if *base-station-p*
      (build-message)
      (build-message :msg-type (getf *msg-types* :node-beacon) :tlv-type (getf *tlv-types* :path))))

(defun generate-message (&key msg-header (msg-type :base-station-beacon) (tlv-type :relay)
			   (tlv-values (list (config-host-address *config*))))
  (let* ((msg-header (or msg-header (make-msg-header :msg-type msg-type)))
	 (tlvs (build-tlvs tlv-values :tlv-type tlv-type))
	 (tlv-block (make-tlv-block tlvs)))
    (sb-concurrency:enqueue (build-packet msg-header tlv-block) *out-buffer*)))

(defun message-hash (field1 field2)
  "Hashing for hashtable key"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~a~a" field1 field2)))))

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

(defun update-duplicate-set (msg-header)
  (with-slots (msg-type msg-orig-addr msg-seq-num) msg-header
    (setf (gethash (message-hash msg-type msg-orig-addr) *duplicate-set*)
	  (make-instance 'duplicate-tuple :orig-addr msg-orig-addr :msg-type msg-type :seq-num msg-seq-num
			 :exp-time (dt:second+ (dt:now) (config-dup-hold-time *config*))))))

(defun process-message (pkt-header msg-header tlv-block)
  (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
    (let ((link-tuple (update-link-set orig-addr))
	  (duplicate-tuple (update-duplicate-set msg-header))) ; add message to duplicate set
      (incf *messages-received*)
      ;; update message and enqueue
      ;; Base Station sends :relay messages to inform the nodes of its presence
      ;; Nodes send :path messages to inform base station of their presence
      (when (and (= msg-type (getf *msg-types* :base-station-beacon)) (not *base-station-p*))
	;(setf (gethash (message-hash (value tlv) orig-addr) *routing-table*) (make-rt-entry :destination (usocket:hbo-to-dotted-quad (value tlv)) :next-hop (usocket:hbo-to-dotted-quad orig-addr) :hop-count hop-count :seq-num seq-num))
	(setf orig-addr (usocket:host-byte-order (config-host-address *config*)))
	(incf hop-count)
	(decf hop-limit))
      (when (and (= msg-type (getf *msg-types* :node-beacon)) *base-station-p*)
       ;(gethash (message-hash (value tlv) orig-addr) *routing-table*) (make-rt-entry :destination (usocket:hbo-to-dotted-quad (value tlv)) :next-hop )
	t) ;; INCOMPLETE
					; (generate-message :pkt-header pkt-header :msg-header msg-header :tlv tlv)
      (format nil "~A DUP --> exp-time: ~A | hop-count: ~A msg-type: ~A orig-addr: ~A seq-num: ~A content: ~A buff:  ~A" (dt:now) (slot-value duplicate-tuple 'exp-time) hop-count msg-type (usocket:hbo-to-dotted-quad orig-addr) seq-num (tlv tlv-block) (usocket:hbo-to-dotted-quad (msg-orig-addr msg-header))))))

(defun retrieve-message (buffer)
  "Unserialize the message and check if it should be processed."
  (multiple-value-bind (pkt-header msg-header tlv-block)
      (unserialize-packet buffer)
    (format t "~A" (tlv tlv-block))
    (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) msg-header
      (cond
	((= hop-limit 0) nil) ; discard
	((= hop-count 255) nil) ; discard
	;((equal (usocket:hbo-to-dotted-quad orig-addr) (config-host-address *config*)) nil) ; discard
	((check-duplicate-set orig-addr seq-num) (format nil "~A" (dt:now))) ; discard
	((not (member msg-type *msg-types*)) nil) ;discard
	(t (process-message pkt-header msg-header tlv-block))))))

(defun out-buffer-get ()
  (let ((buf (userial:make-buffer)))
    (userial:with-buffer buf
      (let ((packet (sb-concurrency:dequeue *out-buffer*)))
	(when packet
	  (serialize-packet packet))))))

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
