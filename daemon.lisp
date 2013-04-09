;;; -*- Mode: Lisp -*-

(in-package :chopin-routing)

(defparameter *this-node* nil)
(defparameter *socket* nil)

(defun init-node ()
  (setf *this-node* (make-instance 'node :address "193.136.230.85"
		       :group-id 0 :signal-strength 5)))

(defun init-neighbours (neighbours-list)
  (with-slots (neighbors) *this-node*
    (dolist (neighbour neighbours-list)
      (vector-push neighbour neighbors))))

(defun icmp (target)
  (sb-ext:run-program "/sbin/ping" `("-c 1" ,target) :output *standard-output*))

(defun send-message (target &key (port 1234))
  (setf *socket* (usocket:socket-connect target port :protocol :datagram))
  (let ((buf (userial:make-buffer)))
    (userial:with-buffer buf
      (userial:serialize-slots* (new-announce-msg *this-node*)
				:string msg-type
				:uint8 origin
				:uint8 group-id
				:uint8 hop-count
				:uint8 rn)
      (usocket:socket-send *socket* buf (length buf)))))

(defun receive-message ()
  (usocket:wait-for-input *socket*))

(defclass packet ()
  ((length :initarg :length
	   :accessor packet-length
	   :type '(unsigned-byte 16))
   (sequence-number :initarg :seq-num
		    :accessor seq-num
		    :type '(unsigned-byte 16))
   (message-type :initarg :msg-type
		 :accessor msg-type
		 :type '(unsigned-byte 8))
   (vtime :initarg :vtime
	  :accessor vtime
	  :type '(unsigned-byte 8))
   (message-size :initarg :msg-size
		 :accessor msg-size
		 :type '(unsigned-byte 8))
   (originator :initarg :originator
	       :accessor originator
	       :type '(unsigned-byte 32))
   (ttl :initarg :ttl
	:accessor ttl
	:type '(unsigned-byte 8))
   (hop-count :initarg :hop-count
	      :accessor hop-count
	      :type '(unsigned-byte 8))
   (message-seq-num :initarg :msg-seq-num
		    :accessor msg-seq-num
		    :type '(unsigned-byte 16))
   (message :accessor :message)))

(defun serialize-packet (packet)
  (userial:make-accessor-serializer (:packet pa packet)
				    :uint16 packet-length
				    :uint16 seq-num
				    :uint8 msg-type
				    :uint8 vtime
				    :uint16 msg-size
				    :uint32 originator
				    :uint8 ttl
				    :uint8 hop-count
				    :uint8 msg-seq-num)
  (userial:serialize :packet packet))

(defun unserialize-packet (packet)
  (userial:unserialize :packet :pa packet))

(defun make-packet ()
  (make-instance 'packet
		 :length 120
		 :seq-num 7771
		 :msg-type 7
		 :vtime 0
		 :msg-size 0
		 :originator (usocket:host-byte-order "10.211.55.2")
		 :ttl 1
		 :hop-count 0
		 :msg-seq-num 1))

(defun build-message ()
  (serialize-packet (make-packet)))

(defclass hello ()
  ((reserved :type '(unsigned-byte 16))
   (htime :initarg :htime
	  :accessor htime
	  :type '(unsigned-byte 8))
   (willingness :initarg :willing
		:accessor willing
		:type '(unsigned-byte 8))
   (link-code :initarg :lnk-code
	      :accessor lnk-code
	      :type '(unsigned-byte 8))
   (reserved2 :type '(unsigned-byte 8))
   (link-message-size :initarg :lnk-msg-size
		      :accessor lnk-msg-size)
   (neighbor-if-address :initarg :neigh-if-addr
			:accessor neigh-if-addr
			:type '(unsigned-byte 32))))


;; sockets

(defvar *broadcast-address* "10.211.55.255")
(defvar *host-address* "10.211.55.2")
(defvar *server* nil)
(defvar *server-port* nil)
(defvar *writer-thread* nil)
(defvar *reader-thread* nil)

(defparameter *broadcast-socket* nil)
(defparameter *broadcast-port* 1234)

(defparameter *out* *standard-output*)

(defun bootstrap ()
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
	 (usocket (usocket::make-datagram-socket socket)))
    (setf (usocket:socket-option usocket :broadcast) t)
    ;(usocket:socket-connect *broadcast-address* *broadcast-port* :protocol :datagram :element-type '(unsigned-byte 8))
    (setf *broadcast-socket* usocket)))

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
						(usocket:socket-close socket))) :name "Received Thread"))))

(defun run-reader (socket buffer)
  (loop
   (multiple-value-bind (buffer size host port)
       (usocket:socket-receive socket buffer (length buffer))
     (unless (not (equal (usocket:vector-quad-to-dotted-quad host) *host-address*))
       (userial:with-buffer buffer
	 (userial:buffer-rewind)
	 (let ((read (userial:unserialize :packet)))
	   (with-open-file (s "/Users/josesantos/received" :direction :output
			      :if-exists :supersede)
	     (format s "Received ~A (~A) bytes from ~A:~A --> ~A BUFFER: ~A~%" size (userial:buffer-length) (usocket:vector-quad-to-dotted-quad host) port read (userial:get-buffer)))))))))

(defun writer (socket)
  (loop
   (usocket:socket-send socket (serialize-packet (make-packet)) (userial:buffer-length) :host *broadcast-address* :port *broadcast-port*)
   (userial:buffer-rewind)
   (sleep 5)))

(defun stop-server ()
  (let ((threads `(,*writer-thread* ,*reader-thread*)))
    (mapcar #'(lambda (th)
		(let ((cur (shiftf th nil)))
		  (when (and cur (not (eql th (bt:current-thread))))
		    (bt:destroy-thread cur)))) threads)))



