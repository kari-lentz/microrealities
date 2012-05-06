(defsystem my-db
  :description "CL databases library"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "my-db"))))
  :depends-on (cl-ppcre cl-mysql mssql my-env utility))
