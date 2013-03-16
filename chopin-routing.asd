;;;; chopin-routing.asd

(asdf:defsystem #:chopin-routing
  :serial t
  :description "Describe chopin-routing here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
	       #:ironclad)
  :components ((:file "package")
	       (:file "class")
	       (:file "chopin-routing")
	       (:file "context")
	       (:file "viewer")))

(load "/Users/josesantos/quicklisp/local-projects/matlisp-master/build/start.lisp")
