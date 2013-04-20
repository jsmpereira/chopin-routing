;;; -*- Mode: Lisp -*-

(in-package :chopin-routing)

(defparameter *this-node* nil)
(defparameter *socket* nil)

;; stats
(defparameter *messages-received* 0)

(defparameter *tc-interval* 5) ; seconds

(defparameter *msg-seq-num* 0) ; wrap-around is 65535
(defparameter *pkt-seq-num* 0) ; same here
(defparameter *msg-hop-limit* 255) ; wrap-around is 255

(defparameter *base-station-p* nil)

(defparameter *msg-types* '(:base-station-beacon 1 :node-beacon 2))
(defparameter *tlv-types* '(:relay 1 :path 2))

(defparameter *duplicate-set* (make-hash-table :test 'equal))

(defclass duplicate-tuple ()
  ((orig_addr :initarg :orig-addr :accessor orig-addr)
   (seq-num :initarg :seq-num :accessor seq-num)
   (exp-time :initarg :exp-time :accessor exp-time)))

(defun icmp (target)
  (sb-ext:run-program "/sbin/ping" `("-c 1" ,target) :output *standard-output*))

(defclass packet ()
  ((pkt-header :initarg :pkt-header)
   (message :initarg :message)))

(defclass pkt-header ()
  ((version :initarg :version
	   :accessor version
	   :type '(unsigned-byte 4))
   (pkt-flags :initarg :pkt-flags
	      :accessor pkt-flags
	      :type '(unsigned-byte 4))
   (pkt-seq-num :initarg :pkt-seq-num
		:accessor pkt-seq-num
		:type '(unsigned-byte 16))) ; end pkt-header
  (:default-initargs
   :version 0
   :pkt-flags #b1000
   :pkt-seq-num *pkt-seq-num*))

(defclass message ()
  ((msg-header :initarg :msg-header)
   (tlv-block :initarg :tlv-block)))

(defclass msg-header ()
  ((msg-type :initarg :msg-type
	     :accessor msg-type
	     :type '(unsigned-byte 8))
   (msg-flags :initarg :msg-flags
	      :accessor msg-flags
	      :type '(unsigned-byte 4))
   (msg-addr-length :initarg :msg-addr-length
		    :accessor msg-addr-length
		    :type '(unsigned-byte 4))
   (msg-size :initarg :msg-size
	     :accessor msg-size
	     :type '(unsigned-byte 16))
   (msg-orig-addr :initarg :msg-orig-addr
		  :accessor msg-orig-addr
		  :type '(unsigned-byte 32))
   (msg-hop-limit :initarg :msg-hop-limit
		  :accessor msg-hop-limit
		  :type '(unsigned-byte 8))
   (msg-hop-count :initarg :msg-hop-count
		  :accessor msg-hop-count
		  :type '(unsigned-byte 8))
   (msg-seq-num :initarg :msg-seq-num
		:accessor msg-seq-num
		:type '(unsigned-byte 16)))
  (:default-initargs
   :msg-flags #b1111
   :msg-addr-length #b0011
   :msg-size 0
   :msg-orig-addr (usocket:host-byte-order *host-address*)
   :msg-hop-limit *msg-hop-limit*
   :msg-hop-count 0
   :msg-seq-num *msg-seq-num*))

(defclass tlv-block ()
  ((tlv :initarg :tlv)))

(defclass tlv ()
  ((tlv-type :initarg :tlv-type
	     :accessor tlv-type
	     :type '(unsigned-byte 8))
   (tlv-flags :initarg :tlv-flags
	      :accessor tlv-flags
	      :type '(unsigned-byte 8))
   (length :initarg :length
	   :accessor vlength
	   :type '(unsigned-byte 8))
   (value :initarg :value
	  :accessor value
	  :type '(unsigned-byte 32)))
  (:default-initargs
   :tlv-flags #b00010000
   :length 4 ; for 32-bit address
   ))

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

(userial:make-accessor-serializer (:pkt-header ph-instance (make-instance 'pkt-header))
				  :uint8 version+pkt-flags
				  :uint16 pkt-seq-num)

(defun serialize-pkt-header (pkt-header)
  (userial:serialize :pkt-header pkt-header))

(defun unserialize-pkt-header (pkt-header)
  (userial:unserialize :pkt-header :ph-instance pkt-header))

(defun msg-flags+msg-addr-length (msg-header)
  (merge-4bit-fields (msg-flags msg-header) (msg-addr-length msg-header)))

(defun (setf msg-flags+msg-addr-length) (value msg-header)
  (multiple-value-bind (flags length) (extract-4bit-fields value)                  
    (setf (msg-flags msg-header) flags
          (msg-addr-length msg-header) length)))

(userial:make-accessor-serializer (:msg-header mh-instance (make-instance 'msg-header))
				  :uint8 msg-type
				  :uint8 msg-flags+msg-addr-length
				  :uint16 msg-size
				  :uint32 msg-orig-addr
				  :uint8 msg-hop-limit
				  :uint8 msg-hop-count
				  :uint16 msg-seq-num)

(defun serialize-msg-header (msg-header)
  (userial:serialize :msg-header msg-header))

(defun unserialize-msg-header (msg-header)
  (userial:unserialize :msg-header :mh-instance msg-header))

(userial:make-accessor-serializer (:tlv tlv-instance (make-instance 'tlv))
				  :uint8 tlv-type
				  :uint8 tlv-flags
				  :uint8 vlength
				  :uint32 value)

(defun serialize-tlv (tlv)
  (userial:serialize :tlv tlv))

(defun unserialize-tlv (tlv)
  (userial:unserialize :tlv :tlv-instance tlv))

(defun make-pkt-header ()
  (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*)))

(defun make-msg-header ()
  (make-instance 'msg-header :msg-type (getf *msg-types* :base-station-beacon)))

(defun make-tlv (value)
  (make-instance 'tlv :tlv-type (getf *tlv-type* :relay) :value (usocket:host-byte-order value)))

(defun build-message (pkt-header msg-header tlv)
  (userial:buffer-rewind)
  (serialize-msg-header msg-header)
  (serialize-tlv tlv)
  ;; msg-size is size of message including msg-header, that is msg-header+tlv
  (setf (msg-size msg-header) (userial:buffer-length))
  (userial:buffer-rewind)
  (serialize-pkt-header pkt-header)
  (serialize-msg-header msg-header)
  (serialize-tlv tlv))

(defun generate-message ()
  (build-message (make-instance 'pkt-header :pkt-seq-num (incf *pkt-seq-num*))
		 (make-instance 'msg-header :msg-type (getf *msg-types* :base-station-beacon) :msg-seq-num (incf *msg-seq-num*) )
		 (make-instance 'tlv :tlv-type (getf *tlv-types* :relay) :value (usocket:host-byte-order "10.211.55.10"))))

(defun message-hash (msg-type orig-addr)
  "Hashing for duplicate set"
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (format nil "~a~a" msg-type orig-addr)))))

(defun check-duplicate-set (msg-type orig-addr)
  (gethash (message-hash msg-type orig-addr) *duplicate-set*))

(defun process-message (pkt-header msg-header tlv)
  (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-count msg-hop-count) (hop-limit msg-hop-limit)) msg-header
    (incf *messages-received*)
    ;; add message to duplicate set
    (let ((dtuple (make-instance 'duplicate-tuple :orig-addr orig-addr :seq-num seq-num :exp-time (dt:second+ (dt:now) 10))))
      (setf (gethash (message-hash msg-type orig-addr) *duplicate-set*)
	    dtuple)
      (with-accessors ((tlv-type tlv-type) (value value)) tlv
	(format nil "~A DUP --> exp-time: ~A | hop-count: ~A msg-type: ~A orig-addr: ~A seq-num: ~A content-type: ~A content: ~A" (dt:now) (slot-value dtuple 'exp-time) hop-count msg-type (usocket:hbo-to-dotted-quad orig-addr) seq-num tlv-type (usocket:hbo-to-dotted-quad value))))))

(defun retrieve-message ()
  "Unserialize the message and check if it should be processed."
  (userial:buffer-rewind)
  (let ((pkt-header (unserialize-pkt-header (make-instance 'pkt-header)))
	(msg-header (unserialize-msg-header (make-instance 'msg-header)))
	(tlv (unserialize-tlv (make-instance 'tlv))))
    (with-accessors ((msg-type msg-type) (orig-addr msg-orig-addr) (seq-num msg-seq-num) (hop-limit msg-hop-limit) (hop-count msg-hop-count)) msg-header
      (cond
	((= hop-limit 0) nil) ; discard
	((= hop-count 255) nil) ; discard
	((not (member msg-type *msg-types*)) nil) ;discard
	((check-duplicate-set msg-type orig-addr) (format nil "~A" (dt:now))) ; discard
	(t (process-message pkt-header msg-header tlv))))))

;; timer / event scheduling

(defun check-duplicate-holding ()
  (loop for key being the hash-keys in *duplicate-set* using (hash-value val)
	when (dt:time>= (dt:now) (slot-value val 'exp-time))
	do (remhash key *duplicate-set*)))

;; sockets

(defvar *broadcast-address* "10.211.55.255")
(defvar *host-address* "10.211.55.2")
(defvar *server* nil)
(defvar *server-port* nil)
(defvar *writer-thread* nil)
(defvar *reader-thread* nil)

(defparameter *broadcast-socket* nil)
(defparameter *broadcast-port* 1234)

(defun start-server (port)
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
					:local-host usocket:*wildcard-host*
					:local-port port))
	(buffer (make-array 32 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (setf (usocket:socket-option socket :broadcast) t)
    (setf *broadcast-socket* socket)
    (setf *writer-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (writer socket)
						(usocket:socket-close socket))) :name "HELLO Thread"))
    (setf *reader-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (run-reader socket buffer)
						(usocket:socket-close socket))) :name "Received Thread"))
    (sb-ext:schedule-timer (sb-ext:make-timer #'check-duplicate-holding) 10 :repeat-interval 7) :thread t))

(defun run-reader (socket buffer)
  (loop
   (multiple-value-bind (buffer size host port)
       (usocket:socket-receive socket buffer (length buffer))
     (unless (equal (usocket:vector-quad-to-dotted-quad host) *host-address*)
       (userial:with-buffer buffer
	 (userial:buffer-rewind)
	 (let ((read (retrieve-message)))
	   (with-open-file (s "/Users/josesantos/received" :direction :output
			      :if-exists :supersede)
	     (format s "Received ~A (~A) bytes from ~A:~A --> ~A~%" size (userial:buffer-length) host port read))))))))

(defun writer (socket)
  (loop
   (usocket:socket-send socket (generate-message) (userial:buffer-length) :host *broadcast-address* :port *broadcast-port*)
   (userial:buffer-rewind)
   (sleep *tc-interval*)))

(defun stop-server ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer))
  (let ((threads `(,*writer-thread* ,*reader-thread*)))
    (mapcar #'(lambda (th)
		(let ((cur (shiftf th nil)))
		  (when (and cur (not (eql th (bt:current-thread))))
		    (bt:destroy-thread cur)))) threads)))
