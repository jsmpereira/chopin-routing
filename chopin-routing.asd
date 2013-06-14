;;;; chopin-routing.asd

(asdf:defsystem #:chopin-routing
  :serial t
  :description "Chopin Routing"
  :author "José Santos Martins Pereira <jsmpereira@gmail.com>"
  :license ""
  :depends-on (#:sb-concurrency
	       #:cffi
	       #:ironclad
	       #:usocket
	       #:userial
	       #:bordeaux-threads
	       #:simple-date-time)
  :components ((:file "package")
	       (:file "daemon-class")
	       (:file "serialization")
	       (:file "udp-server")
	       (:file "daemon")
	       #-darwin (:file "library")
	       (:module utils
			:components ((:file "gen-topology")))))

#+darwin
(asdf:defsystem #:chopin-sim
  :serial t
  :description "Chopin Routing Simulator."
  :author "José Santos Martins Pereira <jsmpereira@gmail.com>"
  :license ""
  :pathname "sim/"
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
	       (:file "class")
	       (:file "chopin-routing")
	       (:file "viewer")
	       (:file "context")))
