(defpackage :cities
  (:documentation "cities for opengl")
  (:use :cl :my-env :utility)
  (:export :initialize-cities))

(defpackage :star-catalog
  (:documentation "stars for opengl")
  (:use :cl :my-env :utility)
  (:export :initialize-stars))

(defpackage :my-opengl
  (:documentation "opengl wrapper")
  (:use :cl :my-env :utility :gl :astrolib :star-catalog :cities)
  (:export :run))

