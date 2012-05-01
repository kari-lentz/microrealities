(defpackage :my-opengl
  (:documentation "opengl wrapper")
  (:use :cl :my-env :utility)
  (:export :run))

(in-package :my-opengl)

(defparameter *aspect* 1.0)
(defconstant +TWO-PI+ (* 2 PI))
(defconstant +HALF-PI+ (* 0.5 PI))

(defmacro with-gensyms( ( &rest symbol-names ) &body forms)
  `(let (,@(loop for symbol-name in symbol-names collecting
	       `(,symbol-name (gensym))))
     ,@forms))

(defun test-fn(x y z)
  (+ x y z))

(defgeneric call(o))

(defclass test-fo ()
  ((x :initarg :x :reader x :documentation "the x value")
   (y :initarg :y :reader y)
   (z :initarg :z :reader z)))
 
(defun test-fo(x y z)
  (make-instance 'test-fo :x x :y y :z z))

(defmethod print-object((o test-fo) stream)
  (format stream (format nil "(test-fo x:~a y:~a z:~a)" (x o) (y o) (z o))))

(defmethod call((o test-fo))
  (+ (x o) (y o) (z o)))

(defun tree-fn(x y z f)
  (let ((v (funcall f (* 10 x) (* 10 y) (* 10 z))))
    (mapcar (lambda(x) (/ x 10)) v)))

(eval-when (:compile-toplevel :load-toplevel :execute) 
   
  (defun create-member(symbol &key documentation)
    (concatenate 
     'list 
     `(,symbol :initarg ,(to-keyword symbol) :reader ,symbol) 
     (and documentation (list :documentation documentation))))

  (defstruct basic-class-member symbol documentation)

  (defun make-declaration(class-name members &optional base-classes)
    `(defclass ,class-name ,base-classes
       ,(loop for member in members 
	   collecting (create-member (basic-class-member-symbol member) :documentation (basic-class-member-documentation member)))))

  (defun make-positional-constructor(class-name members &optional last-rest-p)
    (flet ((pass-rest(params)
	     (if last-rest-p
		 (concatenate 'list (butlast params) (cons '&rest (last params)))
		 params)))
      
      `(defun ,class-name 
	   ,(pass-rest (loop for member in members collecting (basic-class-member-symbol member)))
	 (make-instance (quote ,class-name) 
			,@(apply #'concatenate 'list (loop for member in members collecting `(,(to-keyword (basic-class-member-symbol member)) ,(basic-class-member-symbol member)))))))) 
    
    (defun make-print-method(class-name members)
      `(defmethod print-object((,class-name ,class-name) stream)
	 (format stream 
		 (format nil 
			 ,(format nil "(~a ~a)" class-name 
				  (join " " 
					(loop for member in members collecting 
					     (format nil "~a:~a" (basic-class-member-symbol member) "~a"))))
			 ,@(loop for member in members collecting
				`(,(basic-class-member-symbol member) ,class-name)))))))

(defmacro define-basic-class(class-name (&rest member-specs))
  (let ((members (loop for member-spec in member-specs collecting (apply #'make-basic-class-member :symbol member-spec))))
    `(progn
       ,(make-declaration class-name members)
       ,(make-positional-constructor class-name members)
       ,(make-print-method class-name members))))

(defun inspect-basic-class()
  (macroexpand-1 
   '(define-basic-class employee 
       ((name :documentation "name of the employee")
	(age)
	(occupation :documentation "occupation for the company")))))

(define-basic-class employee 
    ((name :documentation "name of the employee")
     (age)
     (occupation :documentation "occupation for the company")))

(defmacro define-function-object(function-object-name call-method-name (function-name &rest args))
  `(progn
     (define-basic-class ,function-object-name
	 ,(loop for arg in args collecting `(,arg)))
     (defmethod ,call-method-name((,function-object-name ,function-object-name))
       (,function-name ,@(loop for arg in args collecting `(,arg  ,function-object-name))))))
	 
(defun adder(x y z)
  (+ x y z))

(defun inspect-function-object()
  (macroexpand-1 '(define-function-object adder-object render (adder x y z))))

(define-function-object adder-object call (adder x y z))

(define-basic-class tree-test 
    ((branches)))

(defmethod call((tree-test tree-test))
  (loop for branch in (branches tree-test)
       collecting
       (call branch)))

(defmacro define-tree-class(class-name (&rest base-classes) (&rest member-specs))
  (let ((members (loop for member-spec in member-specs collecting (apply #'make-basic-class-member :symbol member-spec))))
    (let ((members+inheritted (concatenate 'list members (list (make-basic-class-member :symbol 'branches)))))
      `(progn
	 ,(make-declaration class-name members base-classes)
	 ,(make-positional-constructor class-name members+inheritted t)
	 ,(make-print-method class-name members+inheritted)))))

(defun inspect-tree-object()
  (macroexpand-1 '(define-tree-class multiplier (tree-test) 
		   ((value-1)
		    (value-2)))))

(define-tree-class multiplier (tree-test)
    ((value-1)
     (value-2)))

(defmethod call((multiplier multiplier))
  (* (value-1 multiplier) (value-2 multiplier) (apply #'+ (call-next-method))))

(define-basic-class gl-tree 
    ((branches)))

(defmethod render((gl-tree gl-tree))
  (loop for branch in (branches gl-tree)
       do
       (render branch)))

(defgeneric render (gl-object))

(define-function-object vertex render (gl:vertex x y z))
(define-function-object color render (gl:color r g b)) 
(define-function-object normal render (gl:normal x y z))
(define-function-object translate render (gl:translate x y z))
(define-function-object rotate render (gl:rotate theta x y z))

(define-tree-class triangles (gl-tree)())

(defmethod render((triangles triangles))
  (gl:with-primitive :triangles
    (call-next-method)))

(define-tree-class quads (gl-tree)())

(defmethod render((quads quads))
  (gl:with-primitive :quads
    (call-next-method)))

(define-tree-class pushed-matrix (gl-tree)())

(defmethod render((pushed-matrix pushed-matrix))
  (gl:with-pushed-matrix
    (call-next-method)))

(define-tree-class scene (gl-tree) 
    ((field-of-view)
     (min-z)
     (max-z)
     (viewport-width)
     (viewport-height)))

(defmethod render((scene scene))
  (sdl:with-init ()
    (sdl:window (viewport-width scene) (viewport-height scene) :flags sdl:sdl-opengl)
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (gl:viewport 0 0 (viewport-width scene) (viewport-height scene))
    (sdl:enable-key-repeat 500 150)

    (gl::matrix-mode :projection)
    (gl:load-identity)

    (let ((width (* (tan (field-of-view scene)) (min-z scene)))) 
      (let ((height (* *aspect* width)))
	(gl:frustum (- 0.0d0 width) width (- 0.0d0 height) height (min-z scene) (max-z scene))))

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
	     (call-next-method)
	     (gl:flush)
	     (sdl:update-display)))))

(defun display-scene()
  
  (let ((w 50)(h 50) (z 0))
    (render 
     (scene (* PI (/ 5 18)) 1 100 640 480 
	    (translate 0 0 -75)
	    (rotate 30 0 1 0)
	    (quads (color 1 0 0) (vertex (- w) h (- z)) (color 0.5 0.5 0) (vertex (- w) (- h) (- z)) (color 0.0 0.5 0) (vertex w (- h) (- z)) (color 0.0 1.0 0) (vertex w h (- z))))))) 

