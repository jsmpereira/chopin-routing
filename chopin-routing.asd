;;;; chopin-routing.asd

(asdf:defsystem #:chopin-routing
  :serial t
  :description "Describe chopin-routing here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads
	       #:lispbuilder-sdl
	       #:ironclad)
  :components ((:file "package")
	       (:file "chopin-routing")
	       (:file "viewer")))
