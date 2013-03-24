(in-package :my-opengl)

(defconstant +aspect+ 1.0)
(defconstant +TWILIGHT+ (* (/ -0.0 180.0) PI))

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

(defun gl-vector-from-astro-vector(astro-vector)
  (let ((v (astro-vector-eq astro-vector)))
    (vector* (aref v 0) (aref v 1) (aref v 2) 1.0)))

(define-condition bad-arguments (error)
  ((msg :initarg :msg :initform "see specs for GL matrix functions"  :reader msg))
  (:report (lambda (o stream) (format stream "bad argments: ~a" (msg o)))))

(defmacro ROT(xyz (angle &optional (units :radians)) &optional gl-matrix)
  (unless (find xyz '(x y z)) (error 'bad-arguments :msg "xyz something other than [xyz]"))
  (unless (find units '(:radians :degrees)) (error 'bad-arguments :msg "units must be :radians or :degrees"))
  (let ((rot-mat `(,(.sym 'rotate- xyz) 
		    ,(if (eq units :degrees) `(degrees ,angle) angle))))
    (if gl-matrix
	`(*m ,rot-mat ,gl-matrix)
	rot-mat)))

