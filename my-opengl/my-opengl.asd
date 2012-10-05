(defsystem my-opengl
  :description "CL open gl wrapper"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "star-catalog")
     (:file "my-opengl"))))
  :depends-on (lispbuilder-sdl cl-jpeg cl-opengl my-env utility astrolib cl-store))
