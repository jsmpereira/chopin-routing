;;; -*- Mode: Lisp -*-

;; Generate topology for tests

(in-package :chopin-routing)

(defparameter *mac-addrs* (make-hash-table :test #'equal))
(defparameter *net* "192.168.0.")

(defun populate-hash ()
  (with-open-file (stream "~/quicklisp/local-projects/chopin-routing/utils/mac_addrs")
    (loop for line = (read-line stream nil)
	  while line
	  for entry = (cl-ppcre:split " " line)
	  do (setf (gethash (first entry) *mac-addrs*) (second entry)))))

(defun addr (host)
  (concatenate 'string *net* (princ-to-string host)))

(defun get-mac (nodes)
  (loop for node in nodes
	collect (gethash (addr node) *mac-addrs*)))

(defun node-spec (node &rest neighbours)
  (when (zerop (hash-table-count *mac-addrs*))
    (populate-hash))
  (format nil "~%# ~A (~A)~%~{~A~%~}" (addr node) (gethash (addr node) *mac-addrs*) (get-mac neighbours)))

(defun build-topology (test &rest node-specs)
  (let ((filename (concatenate 'string "mac_whitelist_" (princ-to-string test))))
    (with-open-file (stream (format nil "~~/quicklisp/local-projects/chopin-routing/utils/~A" filename) :direction :output :if-exists :append :if-does-not-exist :create)
      (format stream "~{~A~}" node-specs))))

(defun test1 ()
  (build-topology 1
		  (node-spec 5 6)
		  (node-spec 6 5 7)
		  (node-spec 7 6 2)
		  (node-spec 2 7 3)
		  (node-spec 3 2 10)
		  (node-spec 10 3)))

(defun test2 ()
  (build-topology 2
		  (node-spec 5 6 2)
		  (node-spec 6 5 7)
		  (node-spec 7 6)
		  (node-spec 2 5 3)
		  (node-spec 3 2 10)
		  (node-spec 10 3)))

(defun test3.1 ()
  (build-topology 3.1
		  (node-spec 5 2 6)
		  (node-spec 2 5 3)
		  (node-spec 6 5 7)
		  (node-spec 7 6 10)
		  (node-spec 3 2)
		  (node-spec 10 7)))

(defun test3.2 ()
  (build-topology 3.2
		  (node-spec 7 nil 10)
		  (node-spec 3 10 nil)
		  (node-spec 10 3 7)))
