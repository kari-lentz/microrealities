(asdf:defsystem #:my-env
  :description "sets global variables for utility"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:utility)
  :components
  ((:module src
	    :components
	    ((:file "my-env")))))
