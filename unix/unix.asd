(defsystem unix
  :description "thin wrapper for trivial shell and other unix libraries"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "unix"))))
  :depends-on (trivial-shell utility))
