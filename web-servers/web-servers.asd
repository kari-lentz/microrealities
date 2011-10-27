(asdf:defsystem #:web-servers
  :description "runs all web servers"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:asdf #:swank #:sb-bsd-sockets #:web-thdirect-admin)
  :components
  ((:module src
	    :components
	    ((:file "web-servers")))))
