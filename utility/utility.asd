(asdf:defsystem #:utility
  :description "Ame utilitites to make Common Lisp work better"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (cl-mysql mssql cl-ppcre)
  :components
  ((:module src
	    :components
	    ((:file "utility")))))
