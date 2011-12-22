(defpackage :utility
  (:documentation "A for general purpose functions and macros")
  (:use :cl)
  (:export :%
	   :string-to-list
	   :range
	   :lazy-chain-or
	   :lazy-chain-and
	   :chain-and
	   :chain-or
	   :chain-call-f
	   :chain-call
	   :post-fix
	   :debug-print
	   :debug-print-str
	   :with-no-package-lock
	   :make-tree
	   :[]
	   :[nil]
	   :{}
	   :inp
	   :hash-table-keys
	   :hash-table-values
	   :hash-table-contents
	   :~
	   :join
	   :make-ctr
	   :str-to-ubyte
	   :ubyte-to-str
	   :make-nil-str
	   :vmap
	   :.sym
	   :to-qualified-symbol
	   :sym-to-db-var
	   :zip-hash
	   :zip
	   :dts-t
	   :make-dts-now
	   :make-dts
	   :bad-dts-error
	   :make-dts-from-ut
	   :dts-p
	   :dts-ut
	   :dts-parts
	   :dts-dow
	   :dts-long-dow
	   :dts-year
	   :dts-month
	   :dts-long-month
	   :dts-day
	   :dts-hour
	   :dts-minute
	   :dts-second
	   :dts-to-Y/M/D
	   :parse-dts-american
	   :parse-dts-rfc-1123
	   :parse-dts-barcode
	   :dts+
	   :make-barcode-from-dts
	   :remove-nil
	   :remove-unspeced-params
	   :qmap
	   :qfilter
	   :[seq]
	   :format-repl
	   :defeasyclass
	   :to-keyword
	   :merge-files
	   :parse-integer-with-default
	   :dump-string-to-file))

(in-package :utility)

;; barcode dts of form YYYYMMDDHHMMSS
(defconstant +regex-barcode-dts+ (ppcre:create-scanner "^([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})"))

(defconstant +secs-per-day+ (* 3600 24))

(defun %(fmt-str &rest args)
  (with-output-to-string (str-var) (apply #'format str-var fmt-str args)))

(defparameter *I* (lambda(x)x))

(defun string-to-list(str)
  (map 'list *I* str))

(defun range( n &optional (lo 0))
  (labels ((range-t(i acc)
	     (if (< i lo)
		 acc
		 (range-t (1- i) (cons i acc)))))
    (range-t (1- (+ n lo)) nil)))

(defun chain-and(seq)
  (labels 
      ((chain-and-r(acc seq)
	 (if (eq seq nil)
	     acc
	     (and (car seq) (chain-and-r (car seq) (cdr seq))))))
    (chain-and-r nil seq)))

(defun chain-or(seq)
  (if (eq seq nil)
      nil
      (or (car seq) (chain-or (cdr seq)))))
	     
(defun lazy-chain-and(seq)
  (labels 
      ((lazy-chain-and-r(acc seq)
	 (if (eq seq nil)
	     acc
	     (let ((res (funcall (car seq))))
	       (and res (lazy-chain-and-r res (cdr seq)))))))
    (lazy-chain-and-r nil seq)))

(defun lazy-chain-or(seq)
  (if (eq seq nil)
      nil
      (or (funcall (car seq)) (chain-or (cdr seq)))))

(defun chain-call-f( arg &rest funcs)
  (labels ((call (ret funcs)
	     (if funcs
		 (call (funcall (car funcs) ret) (cdr funcs))
		 ret)))
    (call arg funcs)))

(defmacro chain-call(arg func-var &rest exprs)
  `(chain-call-f ,arg ,@(loop for expr in exprs collecting `(lambda(,func-var),expr))))

(defmacro post-fix(expr imperative)
  (let ((ret-name (gensym))) 
    `(let ((,ret-name ,expr)) (progn ,imperative ,ret-name))))
  
(defun debug-print(x &optional (fplace (lambda(x) x)))
  (progn
    (format t "{~a}" (funcall fplace x))
	   x))
(defun debug-print-str(x)
  (progn
    (format t "{~1a}" x)
    x))

(defmacro with-no-package-lock(fnlocked &body frm)
  `(locally (declare (sb-ext:disable-package-locks ,fnlocked))
     ,@frm
     (declare (sb-ext:enable-package-locks ,fnlocked))))
    
(defun make-tree( data-tree &optional (fkey (lambda(x)x)))
  (labels ((make-tree-rec(seq tree-acc acc o-prev)
	     (if (not seq)
		 (and (cons (list (funcall fkey o-prev) (reverse acc)) tree-acc))
		 (let ((o (car seq)))
		   (if (equal (funcall fkey o) (funcall fkey o-prev))
		       (make-tree-rec (cdr seq) tree-acc (cons o acc) o-prev)
		       (make-tree-rec (cdr seq) (cons (list (funcall fkey o-prev) (reverse acc)) tree-acc) (cons o nil) o))))))
     (reverse (make-tree-rec data-tree nil nil (car data-tree)))))
		    
(defmacro destructure-pairs(&rest key-value-pairs)
  (let ((data (loop for (key value) in key-value-pairs collecting (list 'list key value))))
       `(list ,@data)))

(defmacro {} (&rest key-value-pairs)
  (let ((lst-key-value-pairs `(list ,@(loop for (key value) in key-value-pairs collecting (list 'list key value))))
	(sym-ht (gensym)) (sym-key (gensym)) (sym-value (gensym)))
    `(let ((,sym-ht (make-hash-table :test 'equalp)))
       (loop for (,sym-key ,sym-value) in (,@lst-key-value-pairs) do (setf (gethash ,sym-key ,sym-ht) ,sym-value))
       ,sym-ht)))
       
(define-condition key-error 
    (error)
  ((key :initarg :key
	:reader key-error-key)
   (hash :initarg :hash
	 :reader key-error-hash))
  (:report (lambda (condition stream)
             (format stream "key error ~a not in hash ~a"
                     (key-error-key condition) (key-error-hash condition)))))

(defun [] (ht key)
  (multiple-value-bind (value p)(gethash key ht)(unless p (error (make-condition 'key-error :key key :hash ht)))value)) 

(defun [nil] (ht key)
  (handler-case ([] ht key) (key-error () nil)))

(defun set-my-hash(ht key v)
  (setf (gethash key ht) v))

(defsetf [] set-my-hash)

(defgeneric inp (item sequence))

(defmethod inp (key (hash-table hash-table))
  (and (multiple-value-bind (param1 param2) (gethash key hash-table) (if (or param1 param2) T nil)) key))

(defmethod inp (item (list cons))
  (find item list :test #'equalp))

(defun hash-table-keys(hash-table &optional (fmap (lambda(x)x)))
  (loop for key being the hash-keys of hash-table
     collecting (funcall fmap key)))

(defun hash-table-values(hash-table &optional (fmap (lambda(x)x)))
  (loop for value being the hash-values of hash-table
     using (hash-key key)
     collecting (funcall fmap value)))

(defun hash-table-contents(hash-table)
  (loop for value being the hash-values of hash-table
     using (hash-key key)
     collecting (list key value)))
 
(defun ~( regex str)
  (multiple-value-bind (begin-match end-match begin-groups end-groups) (ppcre:scan regex str)
    (if (and begin-match end-match)
	(let ((mv (map 'vector (lambda(x y) (handler-case (subseq str x y) (type-error () nil))) begin-groups end-groups)))
	  (and mv (lambda(n) (if (<= n 0) str (aref mv (1- n)))))))))
      
(defun vmap( seq )
  (map 'vector (lambda(n)n) seq))

(defun join( delim str-list)
  (and str-list (if (cdr str-list) 
		    (reduce (lambda(x y) (% "~a~a~a" x delim y)) str-list)
		    (% "~a" (car str-list)))))

(defun make-ctr( init &optional (delta 1) )
  (lambda()(post-fix init (incf init delta))))
	  
(defun str-to-ubyte( str )
  (map '(vector (unsigned-byte 8)) (lambda(c) (char-code c)) str))

(defun ubyte-to-str( bytes )
  (map 'string (lambda(byt)(code-char byt)) bytes))

(defun make-nil-str( str )
  (if (and (stringp str) (> (length str) 0)) 
      str
      nil))

(defun zip-hash( seq-1 seq-2 )
  (let ((ht (make-hash-table)))
    (loop for (x y) in (mapcar (lambda(x y) (list x y)) seq-1 seq-2) do (setf (gethash x ht) y))
    ht))

(defun zip(&rest lists)
 (labels 
      ((zip-r(acc lists)
	 (if (not (chain-and lists))
	     (reverse acc)
	     (zip-r (cons (mapcar (lambda(list) (car list)) lists) acc) (mapcar (lambda(list) (cdr list)) lists)))))
    (zip-r nil lists)))  

(defun .sym(&rest syms)
  (multiple-value-bind (ret)(intern (apply 'concatenate 'string (loop for sym in syms collecting (symbol-name sym))))ret))

(defun to-qualified-symbol( package name )
  (nth-value 0 (intern (symbol-name name) package)))

(defun to-keyword( sym )
  (nth-value 0 (intern (symbol-name sym) "KEYWORD")))

(defun sym-to-db-var( sym )
  (multiple-value-bind (ret)
      (ppcre:regex-replace "-" (symbol-name sym) "_") 
  ret))

(define-condition bad-dts-error (error)
  ((date-int :initarg :int
	:reader bad-dts-error-int))
  (:report (lambda (condition stream)
             (format stream "bad-dts-error parsing ut: ~a" (bad-dts-error-int condition)))))

(defstruct dts-t ut)

(defparameter *days-of-week* ({} (0 "Mon")(1 "Tue")(2 "Wed")(3 "Thu")(4 "Fri")(5 "Sat")(6 "Sun")))
(defparameter *long-days-of-week* ({} (0 "Monday")(1 "Tuesday")(2 "Wednesday")(3 "Thursday")(4 "Friday")(5 "Saturday")(6 "Sunday")))
(defparameter *months* ({} ("Jan" 1)("Feb" 2)("Mar" 3)("Apr" 4)("May" 5)("Jun" 6)("Jul" 7) ("Aug" 8) ("Sep" 9) ("Oct" 10)("Nov" 11)("Dec" 12)))
(defparameter *long-months* ({} (1 "January")(2 "February")(3 "March")(4 "April")(5 "May")(6 "June")(7 "July") (8 "August") (9 "September") (10 "October")(11 "November")(12 "December")))

(defun make-dts(year month day hour minute second &optional time-zone)
  (make-dts-t :ut (encode-universal-time second minute hour day month year time-zone)))

(defun make-dts-now()
  (make-dts-t :ut (get-universal-time)))

(defun make-dts-from-ut( ut )
  (if (numberp ut) 
      (make-dts-t :ut ut)
      (error (make-condition 'bad-dts-error :int ut)))) 

(defun dts-p( arg )
  (dts-t-p arg))

(defun dts-ut( arg )
  (dts-t-ut arg))

(defun dts-parts( dts &optional tz)
  (decode-universal-time (dts-ut dts) tz))

(defun dts-dow( dts )
  (nth-value 6 (decode-universal-time (dts-ut dts))))

(defun dts-long-dow( dts )
  ([] *long-days-of-week* (dts-dow dts)))

(defun dts-year( dts )
  (nth-value 5 (decode-universal-time (dts-ut dts))))

(defun dts-month( dts )
  (nth-value 4 (decode-universal-time (dts-ut dts))))

(defun dts-long-month( dts )
  ([] *long-months* (dts-month dts)))

(defun dts-day( dts )
  (nth-value 3 (decode-universal-time (dts-ut dts))))

(defun dts-hour( dts )
  (nth-value 2 (decode-universal-time (dts-ut dts))))

(defun dts-minute( dts )
  (nth-value 1 (decode-universal-time (dts-ut dts))))

(defun dts-second( dts )
  (nth-value 0 (decode-universal-time (dts-ut dts))))

(defun parse-dts-american( dts-str )
  (let ((mo (~ "^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4}) ([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2}) ([aApP][mM])$" dts-str))) 
    (when mo
      (let ((h (+ (mod (parse-integer (funcall mo 4)) 12) (if (~ "^[pP]" (funcall mo 7)) 12 0))))
	(make-dts (parse-integer (funcall mo 3)) (parse-integer (funcall mo 1)) (parse-integer (funcall mo 2)) h (parse-integer (funcall mo 5)) (parse-integer (funcall mo 6)))))))

(defun parse-dts-rfc-1123( dts-str )
  (let ((mo (~ "^([A-Za-z]{3}), ([0-9]{1,2}) ([A-Za-z]{3}) ([0-9]{4}) ([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2}) GMT$" dts-str)))
    (when mo
      (make-dts-from-ut (encode-universal-time (parse-integer (funcall mo 7)) (parse-integer (funcall mo 6)) (parse-integer (funcall mo 5)) (parse-integer (funcall mo 2)) ([] *months* (funcall mo 3)) (parse-integer (funcall mo 4))0)))))   

(defun dts-to-Y/M/D(dts)
  (format nil "~4,'0d/~2,'0d/~2,'0d" (dts-year dts) (dts-month dts) (dts-day dts)))

(defun dts+(dts num-days)
  (make-dts-from-ut (+ (dts-ut dts) (* +secs-per-day+ num-days))))

(define-condition barcode-dts-error (error)
  ((dts-str :initarg :dts-str :reader dts-str)
   (error-o :initarg :error-o :reader error-o))
  (:report (lambda(err stream) (format stream "error parsing barcode dts of ~a~%~a" (dts-str err) (error-o err)))))

(defun make-barcode-from-dts( dts )
  (multiple-value-bind (second minute hour day month year) (dts-parts dts) (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun parse-dts-barcode( dts-str )
  (let ((mo (~ +regex-barcode-dts+ dts-str)))
    (if mo
	(handler-case
	    (make-dts (parse-integer (funcall mo 1)) (parse-integer (funcall mo 2)) (parse-integer (funcall mo 3)) (parse-integer (funcall mo 4)) (parse-integer (funcall mo 5)) (parse-integer (funcall mo 6)))
	  (error (err) (error (make-condition 'barcode-dts-error :dts-str dts-str :error-o err))))))) 

(defmethod print-object((arg dts-t) s)
  (handler-case
      (multiple-value-bind (second minute hour date month year) (decode-universal-time (dts-t-ut arg)) (format s "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second))
    (error() (format s "bad object"))))

(defun remove-nil( seq )
  (remove-if-not (lambda(x)x) seq))

(defmacro remove-unspeced-params( &rest params )
  `(remove-nil 
    (list ,@(loop for param in params collecting 
		 `(if ,(.sym param '-p) ,param nil)))))

(defmacro qmap( (&rest args) lambda-expr &rest lists)
  `(mapcar (lambda(,@(loop for arg in args collecting arg)) ,lambda-expr) ,@(loop for list in lists collecting list)))

(defmacro qfilter( (&rest args) filter-form map-form &rest lists)
  `(loop for ,args in (zip ,@(loop for list in lists collecting list))
    when ,filter-form collecting ,map-form))

(defun [seq](sequence begin &optional (end 0 end-p))
  (let ((length (length sequence)))
    (let ((end (if end-p end length)))
      (let ((end (if (and (> end 0) (<= end length)) end (mod end length)))
	    (begin (if (and (>= begin 0) (< begin length)) begin (mod begin length))))
	(subseq sequence begin end)))))

(defun format-repl(arg)
  (if (stringp arg) (% "\"~a\"" arg)))

(defmacro defeasyclass( name (&rest direct-superclasses) (&rest members) )
  (let ((sym-o (gensym))(sym-s (gensym)))
    (list
     `(defclass ,name (,@direct-superclasses)
	(,@(loop for member in members collecting `(,member :initarg ,(to-keyword member) :reader ,member))))
     `(defmethod print-object( (,sym-o ,name) ,sym-s) (format ,sym-s ,(apply #'concatenate 'string (loop for member in members collecting (% "{~a:~a}" (symbol-name member) "~a"))) ,@(loop for member in members collecting `(format-repl (,member ,sym-o)))))
     `(defun ,(.sym 'make- name) ,members (make-instance (quote ,name) ,@(apply #'concatenate 'list (loop for member in members collecting `(,(to-keyword member) ,member))))))))

(defun merge-files( target &rest sources )
  (let ((max-buffer-size 65536))
    (let ((data-buffer (make-array max-buffer-size :element-type '(unsigned-byte 8))))
      (with-open-file (out-stream target :direction :output :if-exists :error :if-does-not-exist :create :element-type '(unsigned-byte 8))
	(loop for source in sources do
	     (with-open-file (in-stream source :element-type '(unsigned-byte 8))
	       (tagbody 
		loop-start
		  (let ((num-bytes (read-sequence data-buffer in-stream :end max-buffer-size)))
		    (write-sequence data-buffer out-stream :end num-bytes)
		    (when (= num-bytes max-buffer-size) (go loop-start))))))))))

(defun parse-integer-with-default( str default )
  (handler-case (parse-integer str)(error() default)))

(defun dump-string-to-file( fp string )
  (with-open-file (stream-out fp :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-sequence string stream-out)))
