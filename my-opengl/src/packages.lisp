(defpackage :my-opengl
  (:documentation "opengl wrapper")
  (:use :cl :my-env :utility :gl :astrolib)
  (:export :run
	   #:x))
