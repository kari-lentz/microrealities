(defsystem my-opengl
  :description "CL open gl wrapper"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "packages")
     (:file "star-catalog" :depends-on ("packages"))
     (:file "cities" :depends-on ("packages"))
     (:file "gl-helper" :depends-on ("packages" "star-catalog" "cities"))
     (:file "my-opengl" :depends-on ("gl-helper")))))
  :depends-on (lispbuilder-sdl cl-jpeg cl-opengl my-env utility cl-store astrolib))
