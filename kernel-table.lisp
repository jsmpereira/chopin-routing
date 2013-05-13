;;; -*- Mode: Lisp -*-

(in-package #:chopin-routing)

;; Call shared library libioct to manage Kernel Routing Table 

(cffi:define-foreign-library libioct
  (:unix "libioct.so"))

(cffi:use-foreign-library libioct)

(defun modify_route (gw dst mask op)
  (cffi:foreign-funcall "modify_route" :string gw
			:string dst
			:string mask
			:string op))
