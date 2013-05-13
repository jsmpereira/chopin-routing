;;; -*- Mode: Lisp -*-

(in-package #:chopin-routing)

;; Call shared library libioct to manage Kernel Routing Table 

(cffi:define-foreign-library libioct
  (:unix "libioct.so"))

(cffi:use-foreign-library libioct)

(defun modify-route (gw dst op)
  "Call to foreign function modify_route. Full netmask is used since we're adding HOST routes."
  (cffi:foreign-funcall "modify_route" :string gw :string dst :string "255.255.255.255" :string op))

(defun add-route (gw dst)
  (modify-route gw dst "SIOCADDRT"))

(defun del-route (gw dst mask)
  (modify-route gw dst "SIOCDELRT"))
