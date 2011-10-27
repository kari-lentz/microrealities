(asdf:defsystem #:thdirect-admin
  :description "Ame utilitites to make Common Lisp work better"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:ironclad #:cl-who #:cl-smtp #:my-env)
  :components
  ((:module src
	    :components
	    ((:file "thdirect-admin")))))
