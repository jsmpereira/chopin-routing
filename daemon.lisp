;;; -*- Mode: Lisp -*-

;;; Project CHOPIN http://chopin.isr.uc.pt

(in-package :chopin-routing)

(defparameter *messages-received* 0)

(defparameter *config* nil)
(defparameter *max-jitter* nil) ; (/ refresh-interval 4)

(defparameter *msg-seq-num* 0) ; wrap-around is 65535
(defparameter *pkt-seq-num* 0) ; same here

(defparameter *base-station-p* nil)

(defparameter *msg-types* '(:base-station-beacon 1 :node-beacon 2 :node-reply 3))
(defparameter *tlv-types* '(:relay 1 :path 2 :link-status 3))
(defparameter *link-status* '(:lost 0 :symmetric 1 :heard 2))

(defparameter *out-buffer* (sb-concurrency:make-queue))

(defparameter *duplicate-set* (make-hash-table :test 'equal))
(defparameter *link-set* (make-hash-table :test 'equal))
(defparameter *routing-table* (make-hash-table :test 'equal))

;; Object Factories

(defun make-pkt-header ()
  (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*)))

(defun make-tlv (value &key (tlv-type :relay))
  (make-instance 'tlv :tlv-type (getf *tlv-types* tlv-type) :value (usocket:host-byte-order value)))

(defun make-tlv-block (tlvs)
  "Return a `tlv-block' composed of TLVS. Mid-way serialization to obtain TLVS-LENGTH."
  (let ((buff (userial:make-buffer)))
    (userial:with-buffer buff
      (dolist (entry tlvs)
	(serialize-tlv entry)))
    ;; tlvs-length is number of octets of tlvs
    (make-instance 'tlv-block :tlvs-length (length buff) :tlvs tlvs)))

(defun make-address-block (addr-list)
  (make-instance 'address-block :addr-list addr-list))

(defun make-address-block-tlv (addrs-status)
  (let ((num-addrs (length addrs-status)))
    (make-instance 'tlv-block :tlvs-length (+ num-addrs 3)  ; + 3 tlv fixed octets (tlv-type + tlv-flags + length)
		   :tlvs (list (make-instance 'tlv :tlv-type (getf *tlv-types* :link-status) :tlv-flags #b00010100
					     :length num-addrs :value addrs-status)))))

(defun build-address-block+tlv ()
  (unless (zerop (hash-table-count *link-set*))
    (let* ((links (alexandria:hash-table-values *link-set*))
	   (addr-list (mapcar #'l-neighbor-iface-addr links))
	   (addr-status (mapcar #'l-status links)))
      (make-addr+tlv :address-block (make-address-block addr-list)
		     :tlv-block (make-address-block-tlv addr-status)))))

(defun make-message (&key msg-header tlv-block addr+tlv)
  (make-instance 'message :msg-header msg-header :tlv-block tlv-block :addr+tlv addr+tlv))

(defun make-packet (message)
  (make-instance 'packet :pkt-header (make-pkt-header)
		 :message message))

;;; Message Building

(defun build-tlvs (tlv-values &key (tlv-type :relay))
  "Return a `list' of `tlv' instances, based on TLV-VALUES."
  (loop for value in tlv-values
	collect (make-tlv value :tlv-type tlv-type)))

(defun build-packet (message)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize :message message)
    ;; (serialize-tlv-block tlv-block)
    ;; msg-size is size of message including msg-header, that is msg-header+tlv-block+(<addr-block><tlv-block>)*
    (setf (msg-size (msg-header message)) (userial:buffer-length))
    (make-packet message)))

(defun generate-message (&key msg-header (msg-type :base-station-beacon) (tlv-type :relay)
			   tlv-block (tlv-values (list (config-host-address *config*))))
  "Enqueue `packet' in *OUT-BUFFER*."
  (let* ((msg-header (or msg-header (make-instance 'msg-header :msg-type msg-type)))
	 (tlvs (unless tlv-block (build-tlvs tlv-values :tlv-type tlv-type)))
	 (tlvblock (or tlv-block (make-tlv-block tlvs))))
    (with-accessors ((orig-addr msg-orig-addr) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
      (incf hop-count)
      (decf hop-limit))
    (sb-concurrency:enqueue (build-packet (make-message :msg-header msg-header :tlv-block tlvblock)) *out-buffer*)
    (sb-thread:signal-semaphore *semaphore*)))

(defun generate-node-beacon ()
  (sb-concurrency:enqueue 
   (build-packet (make-message :msg-header (make-instance 'msg-header :msg-type :node-beacon :msg-hop-limit 1)
			       :addr+tlv (build-address-block+tlv))) *out-buffer*))

(defun generate-base-station-beacon ()
  (sb-concurrency:enqueue
   (build-packet (make-message :msg-header (make-instance 'msg-header :msg-type :base-station-beacon))) *out-buffer*))

(defun new-beacon (msg-type)
  "Enqueue a beacon in *OUT-BUFFER* given `msg-type'."
  (assert (valid-msg-type-p msg-type) ()
	  "Invalid message type.")
  (case msg-type
    (:node-beacon (generate-node-beacon))
    (:node-reply 'nr)
    (:base-station-beacon (generate-base-station-beacon)))
  (sb-thread:signal-semaphore *semaphore*))

(defun message-hash (&rest rest)
  "Generate hash key based on passed arguments."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~{~a~}" rest)))))

;;; Processing

(defun check-duplicate-set (msg-type orig-addr seq-num)
  "Return T if *DUPLICATE-SET* contains an entry for MSG-TYPE and ORIG-ADDR. Otherwise, return NIL."
  (gethash (message-hash msg-type orig-addr seq-num) *duplicate-set*))

(defun link-set-params ()
  (values
   (config-refresh-interval *config*)
   (config-neighb-hold-time *config*)))

(defun update-link-set (message tlv-block)
  "Add or update *LINK-SET* entry. For an existing entry update L-TIME. Otherwise, create new `link-tuple'."
  (multiple-value-bind (ref-interval neighb-holding)
      (link-set-params)
    (let* ((orig-addr (msg-orig-addr (msg-header message)))
	   (l-time (dt:second+ (dt:now) (* neighb-holding ref-interval)))
	   (ls-hash (message-hash orig-addr))
	   (current-link (gethash ls-hash *link-set*)))
      (if (and current-link (equal (l-neighbor-iface-addr current-link) orig-addr))
	  (progn (setf (l-time current-link) l-time)
		 (when (and (addr+tlv message) (symmetric-p (addr+tlv message) orig-addr))
		   (setf (l-status current-link) (getf *link-status* :symmetric))))
	  (progn
	    (setf (gethash ls-hash *link-set*) (make-instance 'link-tuple :neighbor-iface-addr orig-addr
							      :time l-time))
	    (update-routing-table (msg-header message)))))))

(defun symmetric-p (addr+tlv orig-addr)
  (let ((addr-block (addr+tlv-address-block addr+tlv)))
    (with-slots (head mid) addr-block
      (member orig-addr
	      (loop for m in mid
		    collect (usocket:host-byte-order (concatenate 'vector head m)))))))

(defun update-duplicate-set (message)
  "Create a `duplicate-tuple' from MSG-HEADER to be added to *DUPLICATE-SET*."
  (with-slots (msg-type msg-orig-addr msg-seq-num) (msg-header message)
    (setf (gethash (message-hash msg-type msg-orig-addr msg-seq-num) *duplicate-set*)
	  (make-instance 'duplicate-tuple :orig-addr msg-orig-addr :msg-type msg-type :seq-num msg-seq-num
			 :exp-time (dt:second+ (dt:now) (config-dup-hold-time *config*))))))

(defun update-kernel-routing-table (destination gateway iface metric)
  "Call ADD-ROUTE foreign function to update OS routing table."
  #-darwin
  (add-route (usocket:hbo-to-dotted-quad destination) (usocket:hbo-to-dotted-quad gateway) iface metric))

(defun next-hop (msg-header)
  (cond ((= (msg-type msg-header) (getf *msg-types* :base-station-beacon)) (msg-orig-addr msg-header))
	(t 0)))

(defun update-routing-table (msg-header)
  "Create `rt-entry' and add to *ROUTING-TABLE*. DESTINATION is the last of the TLV values in TLV-BLOCK."
  (with-slots (msg-orig-addr msg-seq-num msg-hop-count) msg-header
    (let* ((destination (msg-orig-addr msg-header))
	   (next-hop (next-hop msg-header)))
      (setf (gethash (message-hash destination) *routing-table*)
	    (make-rt-entry :destination (usocket:hbo-to-dotted-quad destination)
			   :next-hop next-hop :hop-count (1+ msg-hop-count) :seq-num msg-seq-num))
      (update-kernel-routing-table destination next-hop (config-interface *config*) (1+ msg-hop-count)))))

(defun del-routing-table (destination)
  (let* ((rt-hash (message-hash (usocket:host-byte-order destination)))
	 (rt-entry (gethash rt-hash *routing-table*)))
    (remhash rt-hash *routing-table*)
    #-darwin
    (del-route (rt-entry-destination rt-entry) "0" (config-interface *config*) (rt-entry-hop-count rt-entry))))

(defun valid-tlv-block-p (tlv-block)
  "Return NIL if `tlv-block' contains invalid tlvs. An invalid tlv contains the current node address or 0.0.0.0."
  (let ((tlvs (mapcar #'(lambda (x)
			  (value x)) (tlv tlv-block))))
    (and (notany #'zerop tlvs) (notany #'host-address-p tlvs))))

(defun process-message (message)
  "Update *ROUTING-TABLE*, *DUPLICATE-SET* and *LINK-SET*. If MSG-TYPE is :BASE-STATION-BEACON broadcast. If MSG-TYPE is :NODE-BEACON unicast to next-hop to Base Station. "
  (let ((msg-type (msg-type (msg-header message)))
	(addr+tlv (addr+tlv message)))
    (incf *messages-received*)
    (update-link-set message addr+tlv)
    (update-duplicate-set message)
    (unless *base-station-p* ; Base Station does not forward messages
      (cond
	((and (= msg-type (getf *msg-types* :base-station-beacon)))
	 (rcvlog (format nil "BSB"))
					;	   (generate-message :msg-header msg-header :msg-type msg-type :tlv-type :relay :tlv-block new-tlv-block)
	 )
	((and (= msg-type (getf *msg-types* :node-beacon)))
	 (rcvlog (format nil "NODE BEACON"))
					;	   (generate-message :msg-header msg-header :msg-type msg-type :tlv-type :path :tlv-block new-tlv-block)
	 )
	(t (rcvlog (format nil "!!!! THIS SHOULD NOT BE REACHED!!!!!")))))))

(defun retrieve-message (buffer size)
  "Unserialize BUFFER and into PKT-HEADER, MSG-HEADER and TLV-BLOCK. Parse MSG-HEADER according to RFC 5444."
  (let* ((packet (unserialize-packet buffer)))
    (when (= size (length buffer)) ;; read size must match unserialized length
      (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) (msg-header (message packet))
	(rcvlog (format nil "~A ~A ~A ~A ~A~%" msg-type orig-addr seq-num hop-limit hop-count))
	(cond
	  ((not (member msg-type *msg-types*)) (rcvlog (format nil "UNRECOGNIZED TYPE"))) ;discard
	  ((= hop-limit 0) nil) ; discard
	  ((= hop-count 255) nil) ; discard
	  ((host-address-p orig-addr) (rcvlog (format nil "TALKING TO SELF"))) ; discard
	  ((check-duplicate-set msg-type orig-addr seq-num) (rcvlog (format nil "DUPLICATE"))) ; discard
	  (t (process-message (message packet))))))))

(defun out-buffer-get ()
  "Dequeue element from *OUT-BUFFER* and serialize it into a PACKET."
  (let ((packet (sb-concurrency:dequeue *out-buffer*)))
    (when packet
      (serialize-packet packet))))

;;; timer / event scheduling

(defun check-duplicate-holding ()
  "Remove *DUPLICATE-SET* entries with expired timestamp."
  (loop for key being the hash-keys in *duplicate-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'exp-time))
	do (remhash key *duplicate-set*)))

(defun check-link-set-validity ()
  "Remote *LINK-SET* entries with expired timestamp."
  (loop for key being the hash-keys in *link-set* using (hash-value link-tuple)
	when (dt:time>= (dt:now) (slot-value link-tuple 'l-time))
	do (progn
	     (remhash key *link-set*)
	     (del-routing-table (l-neighbor-iface-addr link-tuple)))))

(defun start-timers ()
  "Setup and start timers."
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-duplicate-holding :name "Duplicate Set Timer" :thread t) 10 :repeat-interval (config-dup-hold-time *config*))
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-link-set-validity :name "Link Set Timer" :thread t) 10 :repeat-interval (config-neighb-hold-time *config*))
  (sb-ext:schedule-timer (sb-ext:make-timer #'(lambda ()
						(if *base-station-p*
						    (new-beacon :base-station-beacon)
						    (new-beacon :node-beacon)))
					    :thread t :name "Beacon Timer") 0 :repeat-interval (config-refresh-interval *config*))
  (sb-ext:schedule-timer (sb-ext:make-timer #'screen :thread t) 3 :repeat-interval (config-refresh-interval *config*)))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer)))

;;--- TODO(jsmpereira@gmail.com): http://tools.ietf.org/html/rfc5148 Jitter Considerations in MANETs
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
~A~%
------- OUT BUFFER -----~%
~A~%" (print-hash *routing-table*) (print-hash *duplicate-set*) (print-hash *link-set*) (sb-concurrency:list-queue-contents *out-buffer*))))

(defun rcvlog (&rest rest)
  (with-open-file (s (merge-pathnames "received" (user-homedir-pathname)) :direction :output
		     :if-exists :append)
    (format s "~{~A ~}~%" rest)))

;;; util

(defmacro with-hash ((hash k v) &body body)
  `(loop for ,k being the hash-keys in ,hash using (hash-value ,v)
	 ,@body))

(defun load-config (&optional (path "quicklisp/local-projects/chopin-routing/.config"))
  (with-open-file (in (merge-pathnames path (user-homedir-pathname)) :direction :input)
    (let ((conf (read in)))
      (setf *config* (apply #'make-config conf))))
  (setf *max-jitter* (/ (config-refresh-interval *config*) 4)))

(defun valid-msg-type-p (msg-type)
  (getf *msg-types* msg-type))

(defun host-address-p (orig-addr)
  "Return T if ORIG-ADDR equals current node address. Otherwise return NIL."
  (string= (usocket:hbo-to-dotted-quad orig-addr) (config-host-address *config*)))

(defun kernel-table-cleanup ()
  "Loop through *ROUTING-TABLE* and cleanup routing entries from Kernel IP table."
  #-darwin
  (with-hash (*routing-table* k v)
    do (del-route (rt-entry-destination v) "0" (config-interface *config*) (rt-entry-hop-count v))))
