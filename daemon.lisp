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
			   tlv-block (tlv-values (list (config-host-address *config*))))
  "Enqueue `packet' in *OUT-BUFFER*."
  (let* ((msg-header (or msg-header (make-msg-header :msg-type msg-type)))
	 (tlvs (unless tlv-block (build-tlvs tlv-values :tlv-type tlv-type)))
	 (tlvblock (or tlv-block (make-tlv-block tlvs))))
    (with-accessors ((orig-addr msg-orig-addr) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
      (incf hop-count)
      (decf hop-limit)
      (rcvlog (format nil "ENQUEUE ------------- [~A]" (msg-seq-num msg-header) (tlv tlvblock))))
    (sb-concurrency:enqueue (build-packet msg-header tlvblock) *out-buffer*)))

(defun new-beacon (msg-type)
  "Enqueue a beacon in *OUT-BUFFER* given `msg-type'."
  (assert (valid-msg-type-p msg-type))
  (sb-concurrency:enqueue (build-packet (make-msg-header :msg-type msg-type)
					(make-tlv-block (build-tlvs (list (config-host-address *config*))))) *out-buffer*))

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
   (config-host-address *config*)
   (config-refresh-interval *config*)
   (config-neighb-hold-time *config*)))

;(config-params :host-address :refresh-interval :neighb-hold-time)

(defun update-link-set (msg-header tlv-block)
  "Add or update *LINK-SET* entry. For an existing entry update L-TIME. Otherwise, create new `link-tuple'."
  (multiple-value-bind (local-addr ref-interval neighb-holding)
      (link-set-params)
    (let* ((l-time (dt:second+ (dt:now) (* neighb-holding ref-interval)))
	   (ls-hash (message-hash local-addr (msg-orig-addr msg-header)))
	   (current-link (gethash ls-hash *link-set*)))
      (if current-link
	  (setf (l-time current-link) l-time)
	  (setf (gethash ls-hash *link-set*) (make-instance 'link-tuple :local-addr local-addr :neighbor-addr (usocket:hbo-to-dotted-quad (msg-orig-addr msg-header)) :l-time l-time)))
      (update-routing-table msg-header tlv-block))))

(defun update-duplicate-set (msg-header)
  "Create a `duplicate-tuple' from MSG-HEADER to be added to *DUPLICATE-SET*."
  (with-slots (msg-type msg-orig-addr msg-seq-num) msg-header
    (setf (gethash (message-hash msg-type msg-orig-addr msg-seq-num) *duplicate-set*)
	  (make-instance 'duplicate-tuple :orig-addr msg-orig-addr :msg-type msg-type :seq-num msg-seq-num
			 :exp-time (dt:second+ (dt:now) (config-dup-hold-time *config*))))))

(defun update-kernel-routing-table (destination gateway iface metric)
  "Call ADD-ROUTE foreign function to update OS routing table."
  (rcvlog (format nil "KERNEL: iface -> ~A dest -> ~A gw -> ~A metric -> ~A" iface (usocket:hbo-to-dotted-quad destination) (usocket:hbo-to-dotted-quad gateway) metric))
  #-darwin
  (add-route (usocket:hbo-to-dotted-quad destination) (usocket:hbo-to-dotted-quad gateway) iface metric))

(defun update-routing-table (msg-header tlv-block)
  "Create `rt-entry' and add to *ROUTING-TABLE*. DESTINATION is the last of the TLV values in TLV-BLOCK."
  (with-slots (msg-orig-addr msg-seq-num msg-hop-count) msg-header
    (let ((destination (path-destination tlv-block)))
      (setf (gethash (message-hash destination destination) *routing-table*)
	    (make-rt-entry :destination (usocket:hbo-to-dotted-quad destination)
			   :next-hop (tlv tlv-block) :hop-count (1+ msg-hop-count) :seq-num msg-seq-num))
      (rcvlog (format nil "~A ~A" (usocket:hbo-to-dotted-quad msg-orig-addr) (usocket:hbo-to-dotted-quad destination)))
      (if (= msg-orig-addr (next-hop tlv-block))
	  (update-kernel-routing-table destination 0 (config-interface *config*) (1+ msg-hop-count))
	  (update-kernel-routing-table destination (next-hop tlv-block) (config-interface *config*) (1+ msg-hop-count))))))

(defun valid-tlv-block-p (tlv-block)
  "Return NIL if `tlv-block' contains invalid tlvs. An invalid tlv contains the current node address or 0.0.0.0."
  (let ((tlvs (mapcar #'(lambda (x)
			  (value x)) (tlv tlv-block))))
    (and (notany #'zerop tlvs) (notany #'host-address-p tlvs))))

(defun process-message (pkt-header msg-header tlv-block)
  "Update *ROUTING-TABLE*, *DUPLICATE-SET* and *LINK-SET*. If MSG-TYPE is :BASE-STATION-BEACON broadcast. If MSG-TYPE is :NODE-BEACON unicast to next-hop to Base Station. "
  (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
    ;(rcvlog (format nil "[~A] ****** IN ******** ~% hop-count: ~A msg-type: ~A seq-num: ~A content: ~A~%" (usocket:hbo-to-dotted-quad orig-addr) hop-count msg-type seq-num (tlv tlv-block)))
    (incf *messages-received*)
    (update-link-set msg-header tlv-block)
    (update-duplicate-set msg-header)
    (let ((new-tlv-block (make-tlv-block (adjoin (make-tlv (config-host-address *config*)) (tlv tlv-block)))))
      (unless *base-station-p* ; Base Station does not forward messages
	(cond
	  ((and (= msg-type (getf *msg-types* :base-station-beacon)))
	   (rcvlog (format nil "~A FORWARding BS Beacon" (usocket:hbo-to-dotted-quad orig-addr)))
	   (generate-message :msg-header msg-header :msg-type msg-type :tlv-type :relay :tlv-block new-tlv-block))
	  ((and (= msg-type (getf *msg-types* :node-beacon)))
	   (generate-message :msg-header msg-header :msg-type msg-type :tlv-type :path :tlv-block new-tlv-block))
	  (t (rcvlog (format nil "!!!! THIS SHOULD NOT BE REACHED!!!!!"))))))))

(defun retrieve-message (buffer size)
  "Unserialize BUFFER and into PKT-HEADER, MSG-HEADER and TLV-BLOCK. Parse MSG-HEADER according to RFC 5444."
  (multiple-value-bind (pkt-header msg-header tlv-block)
      (unserialize-packet buffer)
    (rcvlog (format nil "<------------- [~A] ~A ~A " (msg-seq-num msg-header) (msg-type msg-header) (usocket:hbo-to-dotted-quad (msg-orig-addr msg-header))))
    (when (= size (length buffer)) ;; read size must match unserialized length
      (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) msg-header
	(cond
	  ((not (valid-tlv-block-p tlv-block)) nil) ; discard
	  ((= hop-limit 0) nil) ; discard
	  ((= hop-count 255) nil) ; discard
	  ((host-address-p orig-addr) (rcvlog (format nil "TALKING TO SELF"))) ; discard
	  ((check-duplicate-set msg-type orig-addr seq-num) (rcvlog (format nil "DUPLICATE"))) ; discard
	  ((not (member msg-type *msg-types*)) (rcvlog (format nil "UNRECOGNIZED TYPE"))) ;discard
	  (t (process-message pkt-header msg-header tlv-block)))))))

(defun out-buffer-get ()
  "Dequeue element from *OUT-BUFFER* and serialize it into a PACKET."
  (let ((packet (sb-concurrency:dequeue *out-buffer*)))
    (when packet
      (rcvlog (format nil "~%DEQUEUE -------------> [~A] ~A BUF: ~A" (msg-seq-num (msg-header (message packet))) (msg-type (msg-header (message packet))) (tlv (tlv-block (message packet)))))
      (serialize-packet packet))))

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
	do (progn
	     (remhash key *link-set*)
	     ;(del-route (rt-entry-destination val) "0" (config-interface *config*) (rt-entry-hop-count val))
	     )))

(defun start-timers ()
  "Setup and start timers."
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-duplicate-holding :thread t) 10 :repeat-interval (* 3 (config-refresh-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'check-link-set-validity :thread t) 10 :repeat-interval (* 3 (config-refresh-interval *config*)))
  (sb-ext:schedule-timer (sb-ext:make-timer #'(lambda ()
						(if *base-station-p*
						    (new-beacon :base-station-beacon)
						    (new-beacon :node-beacon)))
					    :thread t) 0 :repeat-interval (config-refresh-interval *config*))
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
~A~%" (print-hash *routing-table*) (print-hash *duplicate-set*) (print-hash *link-set*))))

(defun rcvlog (&rest rest)
  (with-open-file (s (merge-pathnames "received" (user-homedir-pathname)) :direction :output
		     :if-exists :append)
    (format s "~{~A ~}~%" rest)))

;;; util

(defmacro with-hash (hash &body body)
  `(loop for k being the hash-keys in ,hash using (hash-value v)
	 do ,@body))

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
  (with-hash *routing-table*
    (del-route (rt-entry-destination v) "0" (config-interface *config*) (rt-entry-hop-count v))))
