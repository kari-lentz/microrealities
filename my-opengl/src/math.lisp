(in-package :math)

(defconstant +HALF-PI+ (/ PI 2))
(defconstant +TWO-PI+ (* PI 2))

(defun degrees(v)
  (* PI (/ v 180.0d0)))

(defun to-degrees(v)
  (* (/ v PI) 180.0d0))

(defstruct matrix cols rows data) 

(defun [m](matrix row col)
  (aref (matrix-data matrix) (+ (* (matrix-cols matrix) row) col)))

(defun matrix-access-set(matrix row col value)
  (setf (aref (matrix-data matrix) (+ (* (matrix-cols matrix) row) col)) value))

(defsetf [m] matrix-access-set)
  
(defmacro with-matrix(matrix &body body)
  (with-once-only(matrix)
    (with-gensyms (cols row col)
      `(let ((,cols (matrix-cols ,matrix)))
	 (macrolet ((access(,row ,col)
		      (with-once-only (,row ,col)
			`(aref (+ (* ,,cols ,,row) ,,col)))))
	   ,@body)))))

(defun matrix(rows cols &rest init-values)
  (if init-values
      (make-matrix :cols cols :rows rows :data (make-array (* cols rows) :element-type 'float :initial-contents init-values)) 
      (macrolet ((base()
		   `(make-matrix :cols cols :rows rows :data (make-array (* cols rows) :element-type 'float :initial-element 0.0d0))))
	(if (= cols rows)
	    (let ((ret (base)))
	      (for-each-range (col cols)
		(setf ([m] ret col col) 1.0d0))
	      ret)
	    (base)))))

(defmethod print-object((matrix matrix) stream)
  (let ((cols (matrix-cols matrix))(rows (matrix-rows matrix)))
    (format stream "rows: ~a cols: ~a~%" rows cols)
    (format stream
	    (join (% "~%")
		  (map-range (row rows)
			     (join " "
				   (map-range (col cols)
					      (% "~6$" ([m] matrix row col)))))))))
   
(defun matrix-multiply-reverse(matrix-r matrix-l)
  (let ((ret (matrix (matrix-rows matrix-l) (matrix-cols matrix-r))))
    (for-each-range (row (matrix-rows ret))
      (for-each-range (col (matrix-cols ret))
	(let ((total 0.0d0))
	  (for-each-range (idx (matrix-cols matrix-l))
	    (incf total (* ([m] matrix-l row idx) ([m] matrix-r idx col))))
	  (setf ([m] ret row col) total))))
	ret))

(defun *m(&rest matrices)
  (reduce #'matrix-multiply-reverse (reverse matrices)))

(defmacro define-matrix(function params &rest values)
  (let ((row-count (floor (sqrt (length values))))) 
    `(defun ,function
	 ,(loop for param in params collecting
	       (if (atom param) param (second param)))
       (let ,(loop for param in params collecting
		  (if (atom param)
		      `(,param ,param)
		      (destructuring-bind (alias param) param 
			`(,alias ,param))))
	 (matrix ,row-count ,row-count ,@(loop for v in values 
			  collecting 
			    (if (atom v)
				(if (numberp v)
				    (if (floatp v) v (float v))
				    v)
				v)))))))

(defmacro define-matrices(params matrix-specs)
  `(progn
     ,@(loop for matrix-spec in matrix-specs collecting
	    (destructuring-bind (function &rest values) matrix-spec
	      `(define-matrix ,function ,params ,@values)))))

(define-matrices  
    ((? theta))
    ((rotate-x 1 0 0 0
	       0 (cos ?) (- (sin ?)) 0
	       0 (sin ?) (cos ?) 0
	       0 0 0 1)
     (rotate-y (cos ?) 0 (sin ?) 0
	       0 1 0 0
	       (- (sin ?)) 0 (cos ?) 0
	       0 0 0 1)
     (rotate-z (cos ?) (- (sin ?)) 0 0
	       (sin ?) (cos ?) 0 0
	       0 0 1 0
	       0 0 0 1)))

(defun mscale(k matrix)
  (let ((ret (matrix (matrix-rows matrix) (matrix-cols matrix))))
    (for-each-range (row (matrix-rows ret))
      (for-each-range (col (matrix-cols ret))
	(setf ([m] ret row col) (* k ([m] matrix row col)))))
    ret))
    
(defmacro vector*(&rest args)
  `(matrix ,(length args) 1 ,@args))

(defmacro define-element-accessor-macro(&rest elements)
  `(defmacro ,(apply #'.sym 'with '- elements) (,elements matrix &body body) 
     (with-gensyms(data)
	`(with-readers ((,data matrix-data)) ,matrix
	  (let (,,@(let ((idx 0))
		       (loop for element in elements collecting 
			    (post-fix ``(,,element (aref ,data ,',idx))
				      (incf idx)))))
	    ,@body)))))

(define-element-accessor-macro x y z)
(define-element-accessor-macro x y z w)

(defun test-meta-macro()								       
  (macroexpand-1 '(define-element-accessor-macro x y z)))

(defun cartesian(x y z &optional (w 1.0d0))
  (vector* x y z w))

(defun spherical(radius theta phi)
  (let ((sin-theta (sin theta)))
    (vector*
     (* radius sin-theta (cos phi))
     (* radius sin-theta (sin phi))
     (* radius (cos theta)))))

(defun celestial(radius dec ra)
  (spherical radius (- +HALF-PI+ dec) (* (/ ra 24.0d0) +TWO-PI+)))

(defmacro with-spherical((radius theta phi) cartesian &body body)
  (with-gensyms (x y z)
    (with-once-only (cartesian) 
      `(with-xyz (,x ,y ,z) ,cartesian
	 (let ((,radius (sqrt (+ (* ,x ,x) (* ,y ,y) (* ,z ,z)))))
	   (let ((,theta (acos (/ ,z ,radius)))
		 (,phi (atan ,y ,x)))
	     ,@body))))))
 
(defmacro with-celestial((radius dec ra) cartesian &body body)
  `(with-spherical(,radius ,dec ,ra) ,cartesian 
     (let ((,dec (- +HALF-PI+ ,dec))(,ra (* (/ ,ra PI) 12.0d0))) 
       ,@body)))

(defun test-mat ()
  (macroexpand-1 '(define-matrix rotate-x ((? theta)) 
		   1 0 0 0
		   0 (cos ?) (- (sin ?)) 0
		   0 (sin ?) (cos ?) 0
		   0 0 0 1)))
