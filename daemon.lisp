;;; -*- Mode: Lisp -*-

;;; Project CHOPIN http://chopin.isr.uc.pt

(in-package :chopin-routing)

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

;; Object Factories

(defun make-pkt-header ()
  (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*)))

(defun make-msg-header (&key (msg-type :base-station-beacon))
  (make-instance 'msg-header :msg-type (getf *msg-types* msg-type) :msg-seq-num (incf *msg-seq-num*)))

(defun make-tlv (value &key (tlv-type :relay))
  (make-instance 'tlv :tlv-type (getf *tlv-types* tlv-type) :value (usocket:host-byte-order value)))

(defun make-tlv-block (tlvs)
  "Return a `tlv-block' composed of TLVS. Mid-way serialization to obtain TLVS-LENGTH."
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
  "Return a `list' of `tlv' instances, based on TLV-VALUES."
  (loop for value in tlv-values
	collect (make-tlv value :tlv-type tlv-type)))

(defun build-packet (msg-header tlv-block)
  (userial:with-buffer (userial:make-buffer)
    (serialize-msg-header msg-header)
    (serialize-tlv-block tlv-block)
    ;; msg-size is size of message including msg-header, that is msg-header+tlv-block
    (setf (msg-size msg-header) (userial:buffer-length))
    (make-packet :msg-header msg-header :tlv-block tlv-block)))

(defun generate-message (&key msg-header (msg-type :base-station-beacon) (tlv-type :relay)
			   (tlv-values (list (config-host-address *config*))))
  "Enqueue `packet' in *OUT-BUFFER*."
  (let* ((msg-header (or msg-header (make-msg-header :msg-type msg-type)))
	 (tlvs (build-tlvs tlv-values :tlv-type tlv-type))
	 (tlv-block (make-tlv-block tlvs)))
    (sb-concurrency:enqueue (build-packet msg-header tlv-block) *out-buffer*)))

(defun message-hash (&rest rest)
  "Generate hash key based on passed arguments."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~{~a~}" rest)))))

;;; Processing

(defun check-duplicate-set (msg-type orig-addr)
  "Return T if *DUPLICATE-SET* contains an entry for MSG-TYPE and ORIG-ADDR. Otherwise, return NIL."
  (gethash (message-hash msg-type orig-addr) *duplicate-set*))

;;--- TODO(jsmpereira@gmail.com): When updating Link Set need to rebuild routing table.
(defun update-link-set (msg-orig-addr)
  "Add or update *LINK-SET* entry. For an existing entry update L-TIME. Otherwise, create new `link-tuple'."
  (let* ((local-addr (config-host-address *config*))
	 (ref-interval (config-refresh-interval *config*))
	 (neighb-holding (config-neighb-hold-time *config*))
	 (l-time (dt:second+ (dt:now) (* neighb-holding ref-interval)))
	 (ls-hash (message-hash local-addr msg-orig-addr))
	 (current-link (gethash ls-hash *link-set*)))
    (if current-link ; stopped here. Validity time from OSLR makes sense. Need to update time when entry already exists
	(setf (l-time current-link) l-time)
	(setf (gethash ls-hash *link-set*) (make-instance 'link-tuple :local-addr local-addr :neighbor-addr (usocket:hbo-to-dotted-quad msg-orig-addr) :l-time l-time)))))

(defun update-duplicate-set (msg-header)
  "Create a `duplicate-tuple' from MSG-HEADER to be added to *DUPLICATE-SET*."
  (with-slots (msg-type msg-orig-addr msg-seq-num) msg-header
    (setf (gethash (message-hash msg-type msg-orig-addr) *duplicate-set*)
	  (make-instance 'duplicate-tuple :orig-addr msg-orig-addr :msg-type msg-type :seq-num msg-seq-num
			 :exp-time (dt:second+ (dt:now) (config-dup-hold-time *config*))))))

(defun update-kernel-routing-table (destination iface metric)
  "Call ADD-ROUTE foreign function to update OS routing table."
  (rcvlog (format nil "KERNEL: iface -> ~A dest -> ~A metric -> ~A" iface (usocket:hbo-to-dotted-quad destination) metric))
  (add-route (usocket:hbo-to-dotted-quad destination) "0.0.0.0" iface metric))

;;--- TODO(jsmpereira@gmail.com): Need to filter bogus destinations, such as 0.0.0.0.
(defun update-routing-table (msg-header tlv-block)
  "Create `rt-entry' and add to *ROUTING-TABLE*. DESTINATION is the last of the TLV values in TLV-BLOCK."
  (with-slots (msg-orig-addr msg-seq-num msg-hop-count) msg-header
    (with-slots (tlv) tlv-block
      (let ((destination (value (first (last tlv)))))
	(unless (zerop destination)
	  (setf (gethash (message-hash destination destination) *routing-table*)
		(make-rt-entry :destination (usocket:hbo-to-dotted-quad destination)
			       :next-hop tlv :hop-count msg-hop-count :seq-num msg-seq-num))
	  #-darwin(update-kernel-routing-table destination (config-interface *config*) msg-hop-count))))))

;;--- TODO(jsmpereira@gmail.com): Refactor! This can be more readable and maybe do less.
(defun process-message (pkt-header msg-header tlv-block)
  "Update *ROUTING-TABLE*, *DUPLICATE-SET* and *LINK-SET*. If MSG-TYPE is :BASE-STATION-BEACON broadcast. If MSG-TYPE is :NODE-BEACON unicast to next-hop to Base Station. "
  (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
    (incf *messages-received*)
    (unless *base-station-p*
      (incf hop-count) ; maybe check here if incf/decf values result in packet drop?
      (decf hop-limit))
    (update-routing-table msg-header tlv-block)
    ;; Base Station sends :relay messages to inform the nodes of its presence
    ;; Nodes send :path messages to inform base station of their presence
    (let* ((link-tuple (update-link-set orig-addr))
	  (duplicate-tuple (update-duplicate-set msg-header))
	  (rcv (format nil "~A DUP --> exp-time: ~A | hop-count: ~A msg-type: ~A orig-addr: ~A seq-num: ~A content: ~A~%" (dt:now) (slot-value duplicate-tuple 'exp-time)
	      hop-count msg-type (usocket:hbo-to-dotted-quad orig-addr) seq-num (tlv tlv-block)))) ; add message to duplicate set
      (cond
	;; Forward with unchanged content: BROADCAST
	((= msg-type (getf *msg-types* :base-station-beacon))
	 (setf orig-addr (usocket:host-byte-order (config-host-address *config*)))
	 (rcvlog (format nil "~A FORWARding BS Beacon" orig-addr))
	 ;(generate-message :msg-type msg-type :tlv-type :relay)
	 )
	;; Append current node address and forward to next-hop towards base station: UNICAST
	((and (= msg-type (getf *msg-types* :node-beacon)) (not *base-station-p*)) 
	 (generate-message :msg-type msg-type :tlv-type :path :tlv-values
			   `(,(config-host-address *config*)
			      ,@(mapcar #'(lambda (entry)
					    (usocket:hbo-to-dotted-quad (value entry))) (tlv tlv-block)))))
	(t nil))
      (rcvlog (format nil "SEQ NUM: ~A MSG: ~A" (msg-seq-num msg-header) rcv)))))

(defun retrieve-message (buffer size)
  "Unserialize BUFFER and into PKT-HEADER, MSG-HEADER and TLV-BLOCK. Parse MSG-HEADER according to RFC 5444."
  (multiple-value-bind (pkt-header msg-header tlv-block)
      (unserialize-packet buffer)
    (when (= size (length buffer)) ;; read size must match unserialized length
      (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) msg-header
	(cond
	  ((= hop-limit 0) nil) ; discard
	  ((= hop-count 255) nil) ; discard
	  ((equal (usocket:hbo-to-dotted-quad orig-addr) (config-host-address *config*)) nil) ; discard
	  ((check-duplicate-set orig-addr seq-num) (format nil "~A" (dt:now))) ; discard
	  ((not (member msg-type *msg-types*)) nil) ;discard
	  (t (process-message pkt-header msg-header tlv-block)))))))

(defun out-buffer-get ()
  "Dequeue element from *OUT-BUFFER* and serialize it into a PACKET."
  (let ((packet (sb-concurrency:dequeue *out-buffer*)))
    (when packet (serialize-packet packet))))

;;; timer / event scheduling

(defun check-duplicate-holding ()
  "Remove *DUPLICATE-SET* entries with expired timestamp."
  (loop for key being the hash-keys in *duplicate-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'exp-time))
	do (remhash key *duplicate-set*)))

(defun check-link-set-validity ()
  "Remote *LINK-SET* entries with expired timestamp."
  (loop for key being the hash-keys in *link-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'l-time))
	do (remhash key *link-set*)))

(defun start-timers ()
  "Setup and start timers."
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-duplicate-holding :thread t) 10 :repeat-interval (* 3 (config-refresh-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-link-set-validity :thread t) 60 :repeat-interval (* 3 (config-timer-repeat-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'(lambda ()
						(if *base-station-p*
						    (generate-message)
						    (generate-message :msg-type :node-beacon :tlv-type :path)))
					    :thread t) 5 :repeat-interval (config-refresh-interval *config*))
  (sb-ext:schedule-timer (sb-ext:make-timer #'screen :thread t) 5 :repeat-interval (config-refresh-interval *config*)))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer)))

(defun jitter (time)
  "Add some noise to TIME."
  (dt:second+ time (- (random (float *max-jitter*)))))

;;; debug 

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
~A~%" (print-hash *routing-table*) (print-hash *duplicate-set*) (print-hash *link-set*))))

(defun rcvlog (&rest rest)
  (with-open-file (s (merge-pathnames "received" (user-homedir-pathname)) :direction :output
		     :if-exists :append)
    (format s "~A ~{~A ~}~%" (dt:now) rest)))

;;; util

(defun load-config (&optional (path "quicklisp/local-projects/chopin-routing/.config"))
  (with-open-file (in (merge-pathnames path (user-homedir-pathname)) :direction :input)
    (let ((conf (read in)))
      (setf *config* (apply #'make-config conf))))
  (setf *max-jitter* (/ (config-refresh-interval *config*) 4)))
