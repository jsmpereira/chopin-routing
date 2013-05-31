;;; -*- Mode: Lisp -*-
(in-package :chopin-routing)

;; sockets

(defvar *writer-thread* nil)
(defvar *reader-thread* nil)
(defparameter *broadcast-socket* nil)

(defun start-server ()
  (load-config)
  (let ((socket (usocket:socket-connect nil nil :protocol :datagram
					:local-host usocket:*wildcard-host*
					:local-port (config-port *config*))))
    (setf (usocket:socket-option socket :broadcast) t)
    (setf *broadcast-socket* socket)
    (setf *writer-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (writer socket)
						(usocket:socket-close socket))) :name "WRITER Thread"))
    (setf *reader-thread* (bt:make-thread #'(lambda ()
					      (unwind-protect
						   (reader socket)
						(usocket:socket-close socket))) :name "READER Thread"))
    (start-timers)))

(defun reader (socket)
  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (loop
     (multiple-value-bind (buf size host port)
	 (usocket:socket-receive socket buffer (length buffer))
       (rcvlog (format nil "~%<------------- ~A" host))
       (retrieve-message buf size)))))

(defun writer (socket)
  (loop
   (sleep (random (float *max-jitter*)))
   (let ((out (out-buffer-get)))
     (when out
       (usocket:socket-send socket out (length out) :host (config-broadcast-address *config*) :port (config-port *config*))))
   (sleep (config-refresh-interval *config*))))

(defun stop-server ()
  (setf *out-buffer* (sb-concurrency:make-queue))
  (stop-timers)
  (let ((threads `(,*writer-thread* ,*reader-thread*)))
    (mapcar #'(lambda (th)
		(let ((cur (shiftf th nil)))
		  (when (and cur (not (eql th (bt:current-thread))))
		    (bt:destroy-thread cur)))) threads)))
