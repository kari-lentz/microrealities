(defsystem utility
  :description "CL utility library"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "utility"))))
  :depends-on (cl-ppcre cffi))
