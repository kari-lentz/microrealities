(in-package :my-opengl)

(defconstant +aspect+ 1.0)
(defconstant +TWO-PI+ (* 2 PI))
(defconstant +HALF-PI+ (* 0.5 PI))

(define-specials ((*viewport-width* 640)
		(*viewport-height* 480)
		(*astro-date* (astro-date-now))
		(*sky-closure* nil)
		(*limiting-magnitude* 3.5)
		(*latitude* 41)
		(*longitude* 85)
		(*fov* 60)
		(*min-z* 1)
		(*max-z* 100)
		(*ratio* 0.98)
		(*limiting-magnitude* 3.5))
    print-specials)

(defmacro color-r(rgb)
  `(ash ,rgb -16))

(defmacro color-g(rgb)
  `(ash (logand ,rgb #x00ff00) -8))

(defmacro color-b(rgb)
  `(logand ,rgb #x0000ff))

(defmacro define-color-array(lo-color-index hi-color-index gap rgb-component)
  `(defparameter ,(.sym '*star-colors- rgb-component '*) (make-array ,(round (1+ (/ (- hi-color-index lo-color-index ) gap))) :element-type 'integer))) 

(define-condition nil-color-index ()())

(defmacro define-color-function(lo-color-index hi-color-index gap)
  `(defun find-rgb(color-index)
     (restart-case
	 (progn
	   (unless color-index (error 'nil-color-index))
	   (let ((min ,lo-color-index)(max ,hi-color-index)(gap ,gap))
	     (flet ((find-component(color-array)
		      (/
		       (min 255
			    (max 0
				 (round
				  (cond ((and (>= color-index min) (< color-index max))
					 (multiple-value-bind (index frac) (floor (/ (- color-index min) gap))
					   (let ((locolor (aref color-array index)) (hicolor (aref color-array (1+ index))))
					     (+ locolor (* frac (- hicolor locolor))))))
					((< color-index min)
					 (- (aref color-array 0) (* (- (aref color-array 1) (aref color-array 0)) (/ (- min color-index) gap))))
					(t
					 (let ((max-dim (1- (length color-array))))
					   (+ (aref color-array max-dim) (* (/ (- (aref color-array max-dim) (aref color-array (1- max-dim))) gap) (- color-index max))))))))) 
		       255.0)))
	       (values (find-component *star-colors-r*) (find-component *star-colors-g*) (find-component *star-colors-b*)))))
       (assign-white ()
	 (values 1.0 1.0 1.0)))))

(defmacro define-star-colors(lo-color-index hi-color-index gap color-specs)
  `(progn
     (define-color-array ,lo-color-index ,hi-color-index ,gap r)
     (define-color-array ,lo-color-index ,hi-color-index ,gap g)
     (define-color-array ,lo-color-index ,hi-color-index ,gap b)
     ,@(with-collector 
	(!push)
	(loop for (color-index rgb) in color-specs do
	     (let ((index (round (/ (- color-index lo-color-index) gap))))
	       (!push `(setf (aref *star-colors-r* ,index) (color-r ,rgb)))
	       (!push `(setf (aref *star-colors-g* ,index) (color-g ,rgb)))
	       (!push `(setf (aref *star-colors-b* ,index) (color-b ,rgb))))))
     (define-color-function ,lo-color-index ,hi-color-index ,gap)))
   	      
(define-star-colors -0.40 2.00 0.05 
		    ((-0.40 #x9bb2ff)(-0.35 #x9eb5ff)(-0.30 #xa3b9ff)(-0.25 #xaabfff)
		     (-0.20 #xb2c5ff)(-0.15 #xbbccff)(-0.10 #xc4d2ff)(-0.05 #xccd8ff)
		     (-0.00 #xd3ddff)(0.05 #xdae2ff)(0.10 #xdfe5ff)(0.15 #xe4e9ff)
		     (0.20 #xe9ecff)(0.25 #xeeefff)(0.30 #xf3f2ff)(0.35 #xf8f6ff)
		     (0.40 #xfef9ff)(0.45 #xfff9fb)(0.50 #xfff7f5)(0.55 #xfff5ef)
		     (0.60 #xfff3ea)(0.65 #xfff1e5)(0.70 #xffefe0)(0.75 #xffeddb)
		     (0.80 #xffebd6)(0.85 #xffe9d2)(0.90 #xffe8ce)(0.95 #xffe6ca)
		     (1.00 #xffe5c6)(1.05 #xffe3c3)(1.10 #xffe2bf)(1.15 #xffe0bb)
		     (1.20 #xffdfb8)(1.25 #xffddb4)(1.30 #xffdbb0)(1.35 #xffdaad)
		     (1.40 #xffd8a9)(1.45 #xffd6a5)(1.50 #xffd5a1)(1.55 #xffd29c)
		     (1.60 #xffd096)(1.65 #xffcc8f)(1.70 #xffc885)(1.75 #xffc178)
		     (1.80 #xffb765)(1.85 #xffa94b)(1.90 #xff9523)(1.95 #xff7b00)
		     (2.00 #xff5200)))

(with-full-eval

  (defun degrees(num-degrees)
    (* (/ num-degrees 180) PI))

  (defun hours(num-hours)
    (* (/ num-hours 24.0) +TWO-PI+))

  (defun spherical-to-cartesian(rad alt az)
    (let ((sin-alt (sin alt))) 
      (values
       (* rad sin-alt (cos az))
       (* rad sin-alt (sin az))
       (* rad (cos alt))))))

(defmacro with-degrees((&rest degrees) &body body)
  `(let ,(mapcar (lambda(degree) `(,degree (degrees ,degree))) degrees) 
     ,@body))

(defmacro from-spherical((x-var y-var z-var radius alt-ang az-ang) &body body) 
 ` (multiple-value-bind (,x-var ,y-var ,z-var) 
       (spherical-to-cartesian ,radius ,alt-ang ,az-ang)  
     ,@body))

(with-full-eval
  (defun normalize(x y z)
    (let ((anti-mag (/ 1 (sqrt (+ (* x x) (* y y) (* z z))))))
      (values (* x anti-mag) (* y anti-mag) (* z anti-mag)))))

(defmacro with-normalized((x-var y-var z-var x y z) &body body)
  ` (multiple-value-bind (,x-var ,y-var ,z-var) 
	(normalize ,x ,y ,z)  
      ,@body))

(defmacro define-with-primitives(&rest primitive-types)
  `(progn
     ,@(loop for primitive-type in primitive-types
	 collecting
	   `(defmacro ,(.sym 'with- primitive-type)(&body body)
	      `(gl:with-primitives ,',(to-keyword primitive-type) ,@body)))))

(define-with-primitives points triangle quad-strip triangle-strip triangle-fan quads)

(defun assign-light(index x y z w)
  (let ((!light (to-keyword (.sym 'light index)))) 
    (light !light :position (vector x y z w))))

(defun assign-texture(texture-id pf)
  (let ((texture-data (map 'vector (lambda(x)x) (jpeg:decode-image pf))))
    (gl:bind-texture :texture-2d texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    ;; these are actually the defaults, just here for reference
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
    (gl:tex-image-2d :texture-2d 0 3 1024 512 0 :bgr :unsigned-byte texture-data)))

(defmacro with-emission(&body body)
  `(progn
     (material :front :ambient-and-diffuse '(0.0 0.0 0.0 0.0))
     ,@body
     (material :front :emission '(0.0 0.0 0.0 0.0))))

(defun set-color-emissive(r g b)
  (material :front :emission (list r g b 0.0)))

(defparameter *texture-id* nil)
(defparameter *texture-maps* "/home/klentz/runtime/my-opengl/")

(defmacro using-texture(texture-id &body body)
  `(let ((*texture-id* ,texture-id))
     (gl:bind-texture :texture-2d *texture-id*)
     ,@body))

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue")))

(defmacro with-frames((&rest sdl-events) &body body)
  `(sdl:with-events ()
    (:quit-event () t)
    ,@sdl-events
    (:idle ()
	   ;; this lets slime keep working while the main loop is running
	   ;; in sbcl using the :fd-handler swank:*communication-style*
	   ;; (something similar might help in some other lisps, not sure which though)
	   ;#+(and sbcl (not sb-thread)) (restartable
					 ;(sb-sys:serve-all-events 0))
					;(restartable (draw))))))

	   #+(and sbcl (not sb-thread)) (restartable
					  (sb-sys:serve-all-events 0))

	   (restartable
	   
	     (gl:clear :color-buffer-bit :depth-buffer-bit)
	     (gl:cull-face :back)
	     
	     (gl::matrix-mode :modelview)
	     (gl:load-identity)
	     ,@body
	     (gl:flush)
					;(format t "opengl context:~a db:~a~%" (sdl:opengl-context-p) (sdl:double-buffered-p))
	     (sdl:update-display)))))

; with-textures
;
; texture-specs are of the form (texture-symbol "path/to/file.bmp")
;
(defmacro with-textures((&rest texture-specs) &body body)
  (let ((texture-symbols (loop for (texture-symbol pf) in texture-specs collecting texture-symbol))) 
    `(destructuring-bind 
	   ,texture-symbols
	 (gl:gen-textures ,(length texture-specs))
       ,@(loop for (texture-symbol pf) in texture-specs 
	    collecting 
	      `(assign-texture ,texture-symbol (if *texture-maps* (merge-pathnames ,pf *texture-maps*) ,pf)))
       ,@body
       (gl:delete-textures (list ,@texture-symbols)))))

(defmacro with-scene((field-of-view min-z max-z &optional (viewport-width 640) (viewport-height 480)) &body body)
  (with-gensyms (width height screen-ratio)  
    `(let ((*fov* ,field-of-view) 
	   (*min-z* ,min-z) 
	   (*max-z* ,max-z) 
	   (*viewport-width* ,viewport-width) 
	   (*viewport-height* ,viewport-height))
       (sdl:with-init (sdl:sdl-init-video)
	 (sdl:window *viewport-width* *viewport-height* :opengl t :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
	 ;; cl-opengl needs platform specific support to be able to load GL
	 ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
	 (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
	 (gl:viewport 0 0 *viewport-width* *viewport-height*)
	 (sdl:enable-key-repeat 500 150)
	 
	 (gl::matrix-mode :projection)
	 (gl:load-identity)
	 
	 (with-degrees (*fov*)
	   (let ((,width (* (tan *fov*) *min-z*))) 
	     (let ((,screen-ratio (/ *viewport-height* *viewport-width*))) 
	       (let ((,height (* ,screen-ratio ,width)))
		 (gl:frustum (- 0 ,width) ,width (- 0 ,height) ,height *min-z* *max-z*)))))

	 (gl:enable :cull-face :lighting :light0 :depth-test :normalize :color-material :texture-2d)

	 ,@body))))

(defun display-scene-quad()  
  (let ((w 50)(h 50) (z 0))
     (with-scene (60 1 100 640 480)
       (assign-light 0 1 0 0 0)
       (color-material :front :ambient-and-diffuse)
       (with-frames ()
	 (translate 0 0 -75)
	 (rotate 30 0 1 0)
	 (with-quads 
	   (color 1 0 0) (vertex (- w) h (- z)) (color 0.5 0.5 0) (vertex (- w) (- h) (- z)) (color 0.0 0.5 0) (vertex w (- h) (- z)) (color 0.0 1.0 0) (vertex w h (- z)))))))

(defun umbrella-points(radius slices)
  (let ((ang-inc (/ +TWO-PI+ slices)))
    (append
     (list 
      (list 0 0 (/ radius 1))) 
     (map-range (ang (1+ slices)) (from-spherical (x y z radius (degrees 30) (* ang ang-inc)) 
			       (list x y z))))))

(defun display-scene-triangle-fan()
  
     (with-scene (60 1 100 640 480)

       (assign-light 0 1 0 0 0)

       (with-frames ()
	 (translate 0 0 -75)
	 (color-material :front :ambient-and-diffuse)
	 (color 0 1 0)

	 (with-triangle-fan 
	   (loop for (x y z) in (umbrella-points 50 16) do
		(with-normalized (x y z x y z)
		  (normal x y z))
		(vertex x y z))))))

(defun display-scene-triangle()
  
     (with-scene (60 1 100 640 480)

       (color-material :front :ambient-and-diffuse)
       (color 0 1 0)
       (assign-light 0 0 1 0 0)

       (with-frames ()
	   (translate 0 0 -75)
	 (with-triangle-fan
	   (vertex 0 0 50)
	   (vertex 35 0 35)
	   (vertex 0 35 35)
	   (vertex -35 35 35)))))

(defun display-scene-quad-strip()
  
     (with-scene (60 1 100 640 480)

       (assign-light 0 1 0 0 0)
       (color-material :front :ambient-and-diffuse)
       (color 0 1 0)
       
       (with-frames ()
	 (translate 0 0 -75)

	 (let ((radius 50)(slices 16))
	   (flet ((make-point(alt az)
		    (from-spherical (x y z radius alt az)
		      (normal x y z)
		    (vertex x y z))))
	     (let ((delta-ang (/ +TWO-PI+ slices)))
	       (with-quad-strip 
		 (for-each-range (n (1+ slices))
		   (make-point (degrees 15) (* n delta-ang))
		   (make-point (degrees 30) (* n delta-ang))))))))))			  

(defun display-scene-points()
  (with-scene (60 1 100 640 480)
    (with-frames ()
      (translate 0 0 -98.0)
      (with-points
	(vertex 10 0 0)
	(vertex 10 10 0)
	(vertex -10 -10 0)))))
		
(with-full-eval		      
  (defstruct sphere-surface x y z az alt)

  (defmethod make-load-form ((self sphere-surface) &optional environment)
    (declare (ignore environment))
    ;; Note that this definition only works because X and Y do not
    ;; contain information which refers back to the object itself.
    ;; For a more general solution to this problem, see revised example below.
    `(make-sphere-surface :x ,(sphere-surface-x self) :y ,(sphere-surface-y self) :z ,(sphere-surface-z self) :az ,(sphere-surface-az self) :alt ,(sphere-surface-alt self))))

(defmacro make-globe-points(radius slices)

  ; performs compile time trig calculations for vertex of a sphere
  ; slices is a compiled constant, radius is 1.0 at compile and mulitplied out at run-time 
  ;

  (let ((slices (eval slices)))
    (unless (numberp slices) (error "in make-globe-points slices needs to evaluate to a literal decimal number - compile time macro"))  
    (let ((width (1+ slices))(height (1- (/ slices 2))))
      (let ((grid (make-array (* width height ) :element-type 'sphere-surface :initial-element (make-sphere-surface :x 0 :y 0 :z 0)))
	    (delta (/ +TWO-PI+ slices)))
	(for-each-range 
	    (j height)
	  (for-each-range 
	      (i width)
	    (from-spherical (x y z 1.0d0 (* (1+ j) delta) (* i delta))
	      (setf (aref grid (+ i (* width j))) (make-sphere-surface :x x :y y :z z :alt (* (1+ j) delta) :az (* i delta))))))
	(with-gensyms (x y v)
	  `(lambda(,x ,y)
	     (unless (and (>= ,x 0)(< ,x ,width)) (error (format nil "make-globe-points: range error x needs to be from 0 to ~a" ,(1- width))))
	     (unless (and (>= ,y 0)(< ,y ,height)) (error (format nil "make-globe-points: range error y needs to be from 0 to ~a" ,(1- height))))
	     (with-slots (x y z alt az) (aref ,grid (+ ,x (* ,y ,width)))
	       (values-list (append (qmap (,v) (* ,v ,radius)(list x y z))(list alt az))))))))))

(defconstant +slices+ 32)

(defmacro with-sphere-points((x y z alt az) radius &body body)

  (with-once-only (radius)
    (with-gensyms(fpoints draw-point)
    
      `(let ((,fpoints (make-globe-points ,radius +slices+))(x-slices (1+ +slices+))(y-slices (1- (/ +slices+ 2))))
					;top triangle fan
     
	 (flet ((,draw-point(,x ,y ,z ,alt ,az)
		  ,@body))
      
	   (flet ((draw-point-from-array(x-idx y-idx)
		    (multiple-value-bind (x y z alt az)(funcall ,fpoints x-idx y-idx)
		      (,draw-point x y z alt az))))
	   
	       (with-triangle-fan
		 (,draw-point 0 0 radius 0 0)
		 (for-each-range (n x-slices)
		   (draw-point-from-array n 0)))
	
	       (with-quad-strip
		 (for-each-range (n (1- y-slices))
		   (for-each-range (m x-slices)
		     (draw-point-from-array m n)
		     (draw-point-from-array m (1+ n)))))
	
	       (with-triangle-fan
		 (,draw-point 0 0 (- radius) PI 0)
		 (for-each-range (n x-slices)
		   (draw-point-from-array (- x-slices n 1) (1- y-slices))))))))))

(defun draw-planet(radius &optional texture-id)

  (flet ((do-draw-with-texture()
	   (using-texture texture-id
	     (with-sphere-points (x y z alt az) radius
	       (tex-coord (/ az +TWO-PI+) (/ alt PI))
	       (normal x y z)
	       (vertex x y z))))
	 (do-draw-with-no-texture()
	   (with-sphere-points (x y z alt az) radius
	     (declare (ignore alt az))
	     (normal x y z)
	     (vertex x y z))))

    (if texture-id
	(do-draw-with-texture)
	(do-draw-with-no-texture))))

(defstruct gl-matrix cols rows values)

(defun make-gl-identity-matrix()
  (let ((dims 4))
    (make-gl-matrix
     :cols dims
     :rows dims
     :values
     (make-array (* dims dims) :element-type 'float
		 :initial-contents 
		 `(
		   1.0d0 0.0d0 0.0d0 0.0d0
			 0.0d0 1.0d0 0.0d0 0.0d0
			 0.0d0 0.0d0 1.0d0 0.0d0
			 0.0d0 0.0d0 0.0d0 1.0d0)))))
  
(defun make-gl-vector(x y z w)
  (make-gl-matrix
   :cols 1 
   :rows 4
   :values
   (make-array 4 :element-type 'float
	       :initial-contents `(,x ,y ,z ,w))))

(defun gl-vector-from-astro-vector(astro-vector)
  (let ((v (astro-vector-eq astro-vector)))
    (make-gl-vector (aref v 0) (aref v 1) (aref v 2) 1.0)))

(defun get-gl-value(gl-matrix row col)
  (aref 
   (gl-matrix-values gl-matrix)
   (+ (* row (gl-matrix-cols gl-matrix)) col)))

(defun set-gl-value(gl-matrix row col value)
  (setf
   (aref 
    (gl-matrix-values gl-matrix)
    (+ (* row (gl-matrix-cols gl-matrix)) col))
   value))

(defsetf get-gl-value set-gl-value)

(defmacro with-x-y-z-aref((x-symbol y-symbol z-symbol) vector &body body)
  (with-once-only (vector)
    `(let ((,x-symbol (aref ,vector 0))(,y-symbol (aref ,vector 1))(,z-symbol (aref ,vector 2))) 
       ,@body)))

(defmacro with-x-y-z((x-symbol y-symbol z-symbol) gl-vector &body body)
  (with-gensyms (!v)
    (with-once-only (gl-vector)
      `(let ((,!v (gl-matrix-values ,gl-vector)))
	 (let ((,x-symbol (aref ,!v 0))(,y-symbol (aref ,!v 1))(,z-symbol (aref ,!v 2))) 
	   ,@body)))))

(defun gl-matrix-multiply (matrix-1 matrix-2)
  (unless (eq (gl-matrix-cols matrix-1) (gl-matrix-rows matrix-2)) (error "matrix multiplcation has left cols mismatching right rows"))
  (let ((rows (gl-matrix-rows matrix-1))(cols (gl-matrix-cols matrix-2)))
    (let ((ret (make-gl-matrix :cols cols :rows rows :values (make-array (* cols rows) :initial-element 0.0))))
      (for-each-range (row rows)
	(for-each-range (col cols)
	  (let ((total 0))
	    (for-each-range (x (gl-matrix-cols matrix-1))
	      (incf total (* (get-gl-value matrix-1 row x) (get-gl-value matrix-2 x col))))
	    (setf (get-gl-value ret row col) total))))
      ret)))

(defun scale-gl-matrix(matrix scaling)
  (let ((cols (gl-matrix-cols matrix))(rows (gl-matrix-rows matrix)))
    (let ((ret (make-gl-matrix :cols cols :rows rows :values (make-array (* cols rows) :initial-element 0.0))))
      (for-each-range (row rows)
	(for-each-range (col cols)
	  (setf (get-gl-value ret row col) (* scaling (get-gl-value matrix row col)))))
      ret)))

(defun *m (&rest matrices)
  (reduce (lambda(x y) (gl-matrix-multiply y x)) (reverse matrices)))

(defun raw-rotate-z(ang)
  (let ((ret (make-gl-identity-matrix)))
    (setf (get-gl-value ret 0 0) (cos ang))
    (setf (get-gl-value ret 0 1) (- (sin ang)))
    (setf (get-gl-value ret 1 0) (sin ang))
    (setf (get-gl-value ret 1 1) (cos ang))
    ret))

(defun raw-rotate-y(ang)
  (let ((ret (make-gl-identity-matrix)))
    (setf (get-gl-value ret 0 0) (cos ang))
    (setf (get-gl-value ret 0 2) (sin ang))
    (setf (get-gl-value ret 2 0) (- (sin ang)))
    (setf (get-gl-value ret 2 2) (cos ang))
    ret))

(defun raw-rotate-x(ang)
  (let ((ret (make-gl-identity-matrix)))
    (setf (get-gl-value ret 1 1) (cos ang))
    (setf (get-gl-value ret 1 2) (- (sin ang)))
    (setf (get-gl-value ret 2 1) (sin ang))
    (setf (get-gl-value ret 2 2) (cos ang))
    ret))

(define-condition bad-arguments (error)
  ((msg :initarg :msg :initform "see specs for GL matrix functions"  :reader msg))
  (:report (lambda (o stream) (format stream "bad argments: ~a" (msg o)))))

(defmacro GL(&rest args)
  (cond 
    ((eq args nil) `(make-gl-identity-matrix))
    ((eq (length args) 3)  `(make-gl-vector ,@(mapcar #'float args) 0.0))
    ((eq (length args) 4)  `(make-gl-vector ,@(mapcar #'float args)))
    (t (error 'bad-arguments)))) 

(defmacro ROT(xyz (angle &optional (units 'radians)) &optional gl-matrix)
  (unless (find xyz '(x y z)) (error 'bad-arguments :msg "xyz something other than [xyz]"))
  (unless (find units '(radians degrees)) (error 'bad-arguments :msg "units must be radians or degrees"))
  (let ((rot-mat `(,(.sym 'raw-rotate- xyz) ,(if (eq units 'degrees) `(degrees ,angle) angle))))
    (if gl-matrix
	`(*m ,rot-mat ,gl-matrix)
	rot-mat)))
 
(defmacro with-star-db( (stars-symbol) &body body)
    `(let ((filter (and *limiting-magnitude* (lambda(star) (<= (magnitude star) *limiting-magnitude*)))))
       (let ((,stars-symbol (initialize-stars filter)))
	 ,@body)))
	
(defun make-sky-closure(latitude longitude fov min-z max-z ratio)
  (with-degrees (latitude longitude fov)
    (let ((sky-matrix (*m
		       (raw-rotate-x (- latitude +HALF-PI+))
		       (raw-rotate-z (- longitude (hours (gst *astro-date*)) +HALF-PI+))))
	       (dot-product (-(cos fov)))
	       (sky-limit (+ (* ratio max-z) (* (- 1 ratio) min-z))))
      (lambda(dec ra action) 
	(from-spherical (x y z 1.0 (- +HALF-PI+ (degrees dec)) (hours ra))
	  (let ((v (*m sky-matrix (make-gl-vector x y z 0.0))))
	    (when (>= dot-product (get-gl-value v 2 0))
	      (funcall action (scale-gl-matrix v sky-limit)))))))))

(defmacro when-visible((x-symbol y-symbol z-symbol)(dec ra) &body body)
  (with-gensyms (!v)
    `(funcall *sky-closure* ,dec ,ra 
	      (lambda(,!v)
		(with-x-y-z (,x-symbol ,y-symbol ,z-symbol) ,!v
		  ,@body)))))

(defmacro with-sky((latitude longitude ratio) &body body)
  `(let ((*latitude* ,latitude)(*longitude* ,longitude)(*ratio* ,ratio))
     (let ((*sky-closure* (make-sky-closure *latitude* *longitude* *fov* *min-z* *max-z* *ratio*)))
       ,@body)))
	      
(defparameter *cr-lf* (format nil "~%"))

(defmethod print-object ((m gl-matrix) stream)
  (let ((values (gl-matrix-values m)))
    (format stream "<gl-matrix>~%~a"
	    (let ((len (length values)))
	      (case len
		(16
		 (let ((idx 0)(rows))
		   (loop
		      (unless (< idx len) (return))
		      (push 
		       (join " "
			     (map-range (n 4) 
					(format nil "~a" (aref values (+ idx n)))))
		       rows)
		      (incf idx 4))
		   (join *cr-lf* (reverse rows))))
		(4
		 (format nil "~a"
			 (join *cr-lf*
			       (map-range (n 4)
					  (format nil "~a" (aref values n))))))
		(otherwise (format nil "illegal dimensions")))))))

(defmacro with-astro-date((year month day &optional (hour 0) (minute 0) (second 0) dst (tz 0)) &body body) 
  `(let ((*astro-date* (astro-date ,year ,month ,day ,hour ,minute ,second ,dst ,tz)))
     ,@body))
	
(defun draw-stars(stars)
  (with-emission
    (with-pushed-matrix
      (loop for star in stars do
	   (with-slots (star-name dec ra magnitude color-index) star
	     (handler-bind
		 ((nil-color-index  (lambda(c) (declare (ignore c))
					;(format t "INVOKING assign-white restart for ~a:~a:~a:~a:~a~%" star-name dec ra magnitude color-index)
					   (invoke-restart 'assign-white))))
	       (when-visible (x y z) (dec ra)
		 (gl:point-size (1+ (- *limiting-magnitude* magnitude)))
		 (with-points
		   (multiple-value-bind (r g b) (find-rgb color-index)
		     (set-color-emissive r g b))
		   (vertex x y z)))))))))
				   
(defun display-globe(&optional (latitude 40) (longitude 80) astro-date)

  (let ((distance 75)(*astro-date* (or astro-date (astro-date-now)))(fov 60)(min-z 5)(max-z 1000))

    (with-scene (fov min-z max-z 640 480)

      (with-textures ((earth "earth.jpg"))

	(with-star-db(stars)
	  
	  (with-frames 
	      ((:key-down-event 
		(:key key) 
		(case key 
		  (:sdl-key-up (incf latitude 5))
		  (:sdl-key-down (incf latitude -5))
		  (:sdl-key-left (incf longitude 5))
		  (:sdl-key-right (incf longitude -5))
		  (:sdl-key-pageup (incf distance 5))
		  (:sdl-key-pagedown (incf distance -5)))
		(format t "~a~%" key)))

	    (with-sky (latitude longitude 0.98)
	      (draw-stars stars))

	    (with-pushed-matrix
	      (translate 0 0 (- distance))
	      (rotate (- latitude 90) 1 0 0) 
	      (rotate longitude 0 0 1)
	      (rotate 270 0 0 1)
	      
	      (with-pushed-matrix
		(rotate (* -15.0 (gst *astro-date*) ) 0 0 1)
		(with-x-y-z (x y z) (gl-vector-from-astro-vector (sun-pos *astro-date*))
		  (assign-light 0 x y z 0)))
	    
	      (rotate -180 0 0 1)
	      (draw-planet 50.0 earth))))))))
	       		           
(defun test-stars()
  (with-star-db (stars)
    (with-sky (45 81 0.99)
      (loop for star in stars
	 do
	   (with-slots (dec ra star-name color-index magnitude) star 
	     (when-visible (x y z)(dec ra) 
	       (format t "~a:~a:~a:~a:~a:~a:~a:~a~%" star-name dec ra x y z color-index magnitude)))))))

(defun i-test-stars()
  (macroexpand-1
   `(with-sky (45 81 60 0.99)
      (format t "~a~%" (when-above-horizon (degrees 30) (hours 12))))))

(defun display-stars(&optional (latitude 40) (longitude 80) astro-date)
  (let ((*limiting-magnitude* 3.5)(*astro-date* (or astro-date (astro-date-now))))
    (with-scene (60 1 100 640 480)
      (with-star-db(stars)
	(with-frames ()
	  (with-sky (latitude longitude 0.98)
	    (draw-stars stars)))))))

(defun find-rogue-stars()
  (with-star-db (stars)
      (sort stars (lambda (star1 star2) (> (magnitude star1) (magnitude star2))))))
	   