(asdf:defsystem #:web-thdirect-admin
  :description "The top hits diect administrative web server"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:thdirect-admin #:hunchentoot)
  :components
  ((:module src
	    :components
	    ((:file "web-thdirect-admin")))))
