;;;; chopin-routing.asd

(asdf:defsystem #:chopin-routing
  :serial t
  :description "Describe chopin-routing here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sb-concurrency
	       #+darwin #:lispbuilder-sdl
	       #:ironclad
	       #:usocket
	       #:userial
	       #:bordeaux-threads
	       #:simple-date-time)
  :components ((:file "package")
	       #+darwin (:file "class")
	       #+darwin (:file "chopin-routing")
	       (:file "daemon-class")
	       (:file "serialization")
	       (:file "udp-server")
	       (:file "daemon")
	       #+darwin (:file "context")
	       #+darwin (:file "viewer")))

#+darwin (load "/Users/josesantos/quicklisp/local-projects/matlisp-master/build/start.lisp")
