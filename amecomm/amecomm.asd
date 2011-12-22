(asdf:defsystem #:amecomm
  :description "runs amecomm server"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:asdf #:swank #:sb-bsd-sockets #:hunchentoot #:cl-who #:ironclad #:drakma #:utility #:my-db #:my-env)
  :components
  ((:module src
	    :components
	    ((:file "amecomm")))))
