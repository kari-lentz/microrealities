(asdf:defsystem #:junk-mail
  :description "code to send junk mail to clients"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:ironclad #:cl-who #:my-db #:cl-smtp #:cl-mime)
  :components
  ((:module src
	    :components
	    ((:file "junk-mail")))))
