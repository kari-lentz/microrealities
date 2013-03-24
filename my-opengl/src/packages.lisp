(defpackage :cities
  (:documentation "cities for opengl")
  (:use :cl :my-env :utility)
  (:export :initialize-cities :name :population :latitude :longitude))

(defpackage :star-catalog
  (:documentation "stars for opengl")
  (:use :cl :my-env :utility)
  (:export :initialize-stars :sid :star-name :ra :dec :magnitude :color-index))

(defpackage :math
  (:documentation "math helper package")
  (:use :cl :my-env :utility)
  (:export :+HALF-PI+ :+TWO-PI+ 
	   :degrees :to-degrees
	   :matrix :vector* :cartesian
	   :[m]
	   :*m
	   :rotate-x :rotate-y :rotate-z
	   :mscale
	   :with-xyz :with-xyzw
	   :with-spherical :with-celestial))

(defpackage :my-opengl
  (:documentation "opengl wrapper")
  (:use :cl :my-env :utility :gl :astrolib :star-catalog :cities :math)
  (:export :run))

