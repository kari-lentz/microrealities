(defsystem my-opengl
  :description "CL open gl wrapper"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "packages")
     (:file "star-catalog")
     (:file "cities")
     (:file "my-opengl" :depends-on ("star-catalog" "cities")))))
  :depends-on (lispbuilder-sdl cl-jpeg cl-opengl my-env utility cl-store astrolib))
