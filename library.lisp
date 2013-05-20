;;; -*- Mode: Lisp -*-

(in-package #:chopin-routing)

;; Call shared library libkernel_routes to manage Kernel Routing Table 

(cffi:define-foreign-library libkernel-routes
  (:unix "~/quicklisp/local-projects/chopin-routing/c/libkernel_routes.so"))

(cffi:use-foreign-library libkernel-routes)

(defun modify-route (dst gw iface metric op)
  "Call to foreign function modify_route. _op_ = 1 add route, otherwise del route."
  (cffi:foreign-funcall "modify_route" :string dst :string gw :string iface :int metric :int op))

(defun add-route (dst gw iface metric)
  (modify-route dst gw iface metric 1))

(defun del-route (dst gw iface metric)
  (modify-route dst gw iface metric 0))
