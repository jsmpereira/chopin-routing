;;; -*- Mode: Lisp -*-

(in-package #:chopin-routing)

;; Call shared library libioct to manage Kernel Routing Table 

(cffi:define-foreign-library libioct
  (:unix "libioct.so"))

(cffi:use-foreign-library libioct)

(defun modify-route (gw dst mask op)
  (cffi:foreign-funcall "modify_route" :string gw :string dst :string mask :string op))

(defun add-route (gw dst mask)
  (modify-route gw dst mask "SIOCADDRT"))

(defun del-route (gw dst mask)
  (modify-route gw dst mask "SIOCDELRT"))
