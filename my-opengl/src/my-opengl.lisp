(defpackage :my-opengl
  (:documentation "opengl wrapper")
  (:use :cl :my-env :utility :gl)
  (:export :run))

(in-package :my-opengl)

(defconstant +aspect+ 1.0)
(defconstant +TWO-PI+ (* 2 PI))
(defconstant +HALF-PI+ (* 0.5 PI))

(defun degrees(num-degrees)
  (* (/ num-degrees 180) PI))

(defun spherical-to-cartesian(rad alt az)
  (let ((sin-alt (sin alt))) 
    (values
     (* rad sin-alt (cos az))
     (* rad sin-alt (sin az))
     (* rad (cos alt)))))

(defmacro from-spherical((x-var y-var z-var) radius alt-ang az-ang &body body) 
 ` (multiple-value-bind (,x-var ,y-var ,z-var) 
       (spherical-to-cartesian ,radius ,alt-ang ,az-ang)  
     ,@body))

(defun normalize(x y z)
  (let ((anti-mag (/ 1 (sqrt (+ (* x x) (* y y) (* z z))))))
    (values (* x anti-mag) (* y anti-mag) (* z anti-mag))))

(defmacro with-normalized((x-var y-var z-var) x y z &body body)
  ` (multiple-value-bind (,x-var ,y-var ,z-var) 
	(normalize ,x ,y ,z)  
      ,@body))

(defmacro with-triangle(&body body)
  `(gl:with-primitives :triangles ,@body))

(defmacro with-quad-strip(&body body)
  `(gl:with-primitives :quad-strip ,@body))

(defmacro with-triangle-strip(&body body)
  `(gl:with-primitives :triangle-strip ,@body))

(defmacro with-triangle-fan(&body body)
  `(gl:with-primitives :triangle-fan ,@body))

(defmacro with-quads(&body body)
  `(gl:with-primitives :quads ,@body))

(defmacro assign-light(index x y z w)
    `(light ,(to-keyword (.sym 'light index)) :position (vector ,x ,y ,z ,w)))

(defmacro with-scene((field-of-view min-z max-z &optional (viewport-width 640) (viewport-height 480)) &body frms)
  (with-gensyms (width height)  
    (with-once-only (field-of-view min-z max-z viewport-width viewport-height)
      `(sdl:with-init ()
	 (sdl:window ,viewport-width ,viewport-height :flags sdl:sdl-opengl)
	 ;; cl-opengl needs platform specific support to be able to load GL
	 ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
	 (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
	 (gl:viewport 0 0 ,viewport-width ,viewport-height)
	 (sdl:enable-key-repeat 500 150)
      
	 (gl::matrix-mode :projection)
	 (gl:load-identity)
      
	 (let ((,width (* (tan ,field-of-view) ,min-z))) 
	   (let ((,height (* +aspect+ ,width)))
	     (gl:frustum (- 0.0d0 ,width) ,width (- 0.0d0 ,height) ,height ,min-z ,max-z)))
       
	 (sdl:with-events ()
	   (:quit-event () t)
	   (:idle ()
		  ;; this lets slime keep working while the main loop is running
		  ;; in sbcl using the :fd-handler swank:*communication-style*
		  ;; (something similar might help in some other lisps, not sure which though)
		  #+(and sbcl (not sb-thread)) (restartable
					      (sb-sys:serve-all-events 0))
					;(restartable (draw))))))
		
					;(gl:enable :texture-2d)
		  (gl:enable :cull-face :lighting :light0 :depth-test :normalize :color-material)
	     
		  (gl:clear :color-buffer-bit :depth-buffer-bit)
		  (gl:cull-face :back)
	     
		  (gl::matrix-mode :modelview)
		  (gl:load-identity)
		  ,@frms
		  (gl:flush)
		  (sdl:update-display)))))))

(defun display-scene-quad()  
  (let ((w 50)(h 50) (z 0))
     (with-scene ((degrees 60) 1 100 640 480)
       (assign-light 0 1 0 0 0)
       (color-material :front :ambient-and-diffuse)
       
       (translate 0 0 -75)
       (rotate 30 0 1 0)
       (with-quads (color 1 0 0) (vertex (- w) h (- z)) (color 0.5 0.5 0) (vertex (- w) (- h) (- z)) (color 0.0 0.5 0) (vertex w (- h) (- z)) (color 0.0 1.0 0) (vertex w h (- z))))))

(defun umbrella-points(radius slices)
  (let ((ang-inc (/ +TWO-PI+ slices)))
    (append
     (list 
      (list 0 0 (/ radius 1))) 
     (map-range (ang (1+ slices)) (from-spherical (x y z)  radius (degrees 30) (* ang ang-inc) 
			       (list x y z))))))

(defun display-scene-triangle-fan()
  
     (with-scene ((degrees 60) 1 100 640 480)

       (assign-light 0 1 0 0 0)
       (translate 0 0 -75)
       (color-material :front :ambient-and-diffuse)
       (color 0 1 0)

       (with-triangle-fan 
	 (loop for (x y z) in (umbrella-points 50 16) do
	      (with-normalized (x y z) x y z
		  (normal x y z))
	      (vertex x y z)))))

(defun display-scene-triangle()
  
     (with-scene ((degrees 60) 1 100 640 480)

       (assign-light 0 0 1 0 0)
       (translate 0 0 -75)
       (color-material :front :ambient-and-diffuse)
       (color 0 1 0)
       (with-triangle-fan
	 (vertex 0 0 50)
	 (vertex 35 0 35)
	 (vertex 0 35 35)
	 (vertex -35 35 35))))

(defun display-scene-quad-strip()
  
     (with-scene ((degrees 60) 1 100 640 480)

       (assign-light 0 1 0 0 0)
       (translate 0 0 -75)
       (color-material :front :ambient-and-diffuse)
       (color 0 1 0)

       (let ((radius 50)(slices 16))
	 (flet ((make-point(alt az)
		  (from-spherical (x y z) radius alt az
		    (normal x y z)
		    (vertex x y z))))
	   (let ((delta-ang (/ +TWO-PI+ slices)))
	     (with-quad-strip 
	       (for-each-range (n (1+ slices))
		 (make-point (degrees 15) (* n delta-ang))
		 (make-point (degrees 30) (* n delta-ang)))))))))			  
				      
