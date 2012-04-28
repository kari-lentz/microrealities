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
  ((x :initarg :x :reader x :documentation "the x value")
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

(eval-when (:compile-toplevel) 
   
  (defun create-member(symbol &key documentation)
    (concatenate 
     'list 
     `(,symbol :initarg ,(to-keyword symbol) :reader ,symbol) 
     (and documentation (list :documentation documentation))))

  (defstruct basic-class-member symbol documentation)

  (defun make-declaration(class-name members)
    `(defclass ,class-name nil 
       ,(loop for member in members 
	   collecting (create-member (basic-class-member-symbol member) :documentation (basic-class-member-documentation member)))))

  (defun make-positional-constructor(class-name members)
    `(defun ,class-name 
	 ,(loop for member in members collecting (basic-class-member-symbol member))
       (make-instance (quote ,class-name) 
		      ,@(apply #'concatenate 'list (loop for member in members collecting `(,(to-keyword (basic-class-member-symbol member)) ,(basic-class-member-symbol member))))))) 

  (defun make-print-method(class-name members)
    `(defmethod print-object((,class-name ,class-name) stream)
       (format stream 
	       (format nil 
		       ,(format nil "\"(~a ~a)\"" class-name 
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

(define-basic-class employee 
    ((name :documentation "name of the employee")
     (age)
     (occupation :documentation "occupation for the company")))