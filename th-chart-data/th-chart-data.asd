(asdf:defsystem #:th-chart-data
  :description "Creates pdf files"
  :version "0.1"
  :author "Kari Lentz <kari_lentz@amemusic.com>"
  :maintainer "Kari Lentz <kari_lentz@amemusic.com>"
  :licence "?"
  :depends-on (#:cl-typesetting #:cl-pdf #:flexi-streams #:my-db)
  :components
  ((:module src
	    :components
	    ((:file "th-chart-data")))))
