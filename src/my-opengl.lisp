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

(defgeneric render(o))

(defclass test-fo ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (z :initarg :z :reader z)))
 
(defun test-fo(x y z)
  (make-instance 'test-fo :x x :y y :z z))

(defmethod print-object((o test-fo) stream)
  (format stream (format nil "(test-fo x:~a y:~a z:~a)" (x o) (y o) (z o))))

(defmethod render((o test-fo))
  (+ (x o) (y o) (z o)))

(defun tree-fn(x y z f)
  (let ((v (funcall f (* 10 x) (* 10 y) (* 10 z))))
    (mapcar (lambda(x) (/ x 10)) v)))