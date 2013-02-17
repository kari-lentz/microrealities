(defsystem my-opengl
  :description "CL open gl wrapper"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "packages")
     (:file "star-catalog" :depends-on ("packages"))
     (:file "cities" :depends-on ("packages"))
     (:file "gl-helper" :depends-on ("packages"))
     (:file "my-opengl" :depends-on ("gl-helper" "star-catalog" "cities")))))
  :depends-on (lispbuilder-sdl cl-jpeg cl-opengl my-env utility cl-store astrolib))
