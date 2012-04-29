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

  (defun make-declaration(class-name members &rest base-classes)
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

(define-function-object adder-object render (adder x y z))

(define-basic-class tree 
    ((branches)))

(defmethod render((tree tree))
  (loop for branch in (branches tree)
       collecting
       (render branch)))

(defmacro define-tree-class(class-name (&rest member-specs))
  (let ((members (loop for member-spec in member-specs collecting (apply #'make-basic-class-member :symbol member-spec))))
    (let ((members+inheritted (concatenate 'list members (list (make-basic-class-member :symbol 'branches)))))
      `(progn
	 ,(make-declaration class-name members 'tree)
	 ,(make-positional-constructor class-name members+inheritted t)
	 ,(make-print-method class-name members+inheritted)))))

(defun inspect-tree-object()
  (macroexpand-1 '(define-tree-class multiplier 
		   ((value-1)
		    (value-2)))))

(define-tree-class multiplier 
    ((value-1)
     (value-2)))

(defmethod render((multiplier multiplier))
  (* (value-1 multiplier) (value-2 multiplier) (apply #'+ (call-next-method))))

