(in-package :my-opengl)

(defconstant +aspect+ 1.0)
(defconstant +TWO-PI+ (* 2 PI))
(defconstant +HALF-PI+ (* 0.5 PI))
(defconstant +TWILIGHT+ (* (/ -18.0) PI))

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

(defmacro with-astro-date((&optional astro-date) &body body)
  (with-once-only (astro-date)
    `(let ((*astro-date* (or ,astro-date (astro-date-now))))
       ,@body)))

(defstruct gl-matrix cols rows values)

(with-full-eval

  (defun radians-to-degrees(num-radians)
    (* (/ num-radians PI) 180.0))

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

(defun spherical-to-geographic(latitude longitude)
  (values (- +HALF-PI+ latitude) longitude))

(defun cartesian-to-spherical(x y z)
  (let ((mag (sqrt (+ (* x x) (* y y) (* z z)))))
    (values (acos (/ z mag))
	    (atan y x))))

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
    ((eq (length args) 3)  `(make-gl-vector ,@(qmap (arg) `(float ,arg) args) 0.0))
    ((eq (length args) 4)  `(make-gl-vector ,@(qmap (arg) `(float ,arg) args)))
    (t (error 'bad-arguments)))) 

(defmacro ROT(xyz (angle &optional (units :radians)) &optional gl-matrix)
  (unless (find xyz '(x y z)) (error 'bad-arguments :msg "xyz something other than [xyz]"))
  (unless (find units '(:radians :degrees)) (error 'bad-arguments :msg "units must be :radians or :degrees"))
  (let ((rot-mat `(,(.sym 'raw-rotate- xyz) 
		    ,(if (eq units :degrees) `(degrees ,angle) angle))))
    (if gl-matrix
	`(*m ,rot-mat ,gl-matrix)
	rot-mat)))

