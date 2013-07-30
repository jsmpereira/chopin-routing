;;; -*- Mode: Lisp -*-
(in-package :chopin-routing)

;; sockets

(defvar *writer-thread* nil)
(defvar *reader-thread* nil)
(defvar *replier-thread* nil)
(defvar *semaphore* nil)
(defparameter *broadcast-socket* nil)

(defun start-server ()
  (load-config)
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
					:local-host usocket:*wildcard-host*
					:local-port (config-port *config*))))
    (setf (usocket:socket-option socket :broadcast) t)
    (setf *broadcast-socket* socket)
    (setf *semaphore* (sb-thread:make-semaphore))
    (setf *writer-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (writer socket)
						(usocket:socket-close socket))) :name "WRITER Thread"))
    (setf *reader-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (reader socket)
						(usocket:socket-close socket))) :name "READER Thread"))
    (setf *replier-thread* (bt:make-thread #'(lambda ()
    					       (unwind-protect
    						    (replier socket)
    						 (usocket:socket-close socket))) :name "REPLIER Thread"))
    (start-timers)))

(defun replier (socket)
  (loop
   (multiple-value-bind (packet destination) (reply-buffer-get)
     (when packet
       (usocket:socket-send socket packet (length packet) :host (usocket:hbo-to-dotted-quad destination)
			    :port (config-port *config*))))))

(defun reader (socket)
  (loop
   (multiple-value-bind (buf size host port)
       (usocket:socket-receive socket (make-array 128 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
     (unless (host-address-p (usocket:host-byte-order host))
       (rcvlog (format nil "BUF: ~A ~A => ~A" buf size host))
       (retrieve-message buf size)))))

(defun writer (socket)
  (loop
   (let ((out (out-buffer-get)))
     (when out
       (usocket:socket-send socket out (length out) :host (config-broadcast-address *config*)
			    :port (config-port *config*))))
   (sb-thread:wait-on-semaphore *semaphore*)))

(defun stop-server ()
  (setf *out-buffer* (sb-concurrency:make-queue))
  (stop-timers)
  (let ((threads `(,*writer-thread* ,*reader-thread*)))
    (mapcar #'(lambda (th)
		(let ((cur (shiftf th nil)))
		  (when (and cur (not (eql th (bt:current-thread))))
		    (bt:destroy-thread cur)))) threads)))
