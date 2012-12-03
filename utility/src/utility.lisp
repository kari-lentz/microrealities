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
	   :dts-to-rfc-1123
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
	   :defunitclass
	   :deflistwalker
	   :merge-files
	   :parse-integer-with-default
	   :dump-string-to-file
	   :with-gensyms
	   :with-once-only
	   :for-each-range
	   :map-range
	   :replace-symbol
	   :map-symbol
	   :with-rebind
	   :repeat-apply
	   :filled-list
	   :with-full-eval
	   :define-specials
	   :with-collector
	   :aif
	   :it
	   :map-rows
	   :with-regexes
	   :get-first-atom
	   :with-regex-matches
	   :with-rebindings))

(in-package :utility)

;; barcode dts of form YYYYMMDDHHMMSS
(defparameter *regex-barcode-dts* (ppcre:create-scanner "^([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})"))

(defparameter *secs-per-day* (* 3600 24))

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
  (nth-value 0 (intern (with-output-to-string (stream) (loop for string in syms do (princ string stream))))))

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
(defparameter *short-months* ({} (1 "Jan")(2 "Feb")(2 "Mar")(4 "Apr")(5 "May")(6 "Jun")(7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct")(11 "Nov")(12 "Dec")))
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

(defun dts-to-rfc-1123 (&optional (dts (make-dts-now)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (dts-parts dts 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            ([] *days-of-week* day-of-week)
            date
            ([] *short-months* month)
            year
            hour
            minute
            second)))

(defun dts-to-Y/M/D (&optional (dts (make-dts-now)))
  (format nil "~4,'0d/~2,'0d/~2,'0d" (dts-year dts) (dts-month dts) (dts-day dts)))

(defun dts+(dts num-days)
  (make-dts-from-ut (+ (dts-ut dts) (* *secs-per-day* num-days))))

(define-condition barcode-dts-error (error)
  ((dts-str :initarg :dts-str :reader dts-str)
   (error-o :initarg :error-o :reader error-o))
  (:report (lambda(err stream) (format stream "error parsing barcode dts of ~a~%~a" (dts-str err) (error-o err)))))

(defun make-barcode-from-dts( dts )
  (multiple-value-bind (second minute hour day month year) (dts-parts dts) (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun parse-dts-barcode( dts-str )
  (let ((mo (~ *regex-barcode-dts* dts-str)))
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
  (if (stringp arg) 
      (format nil "\"~a\"" arg)
      arg))

(defmacro defunitclass(type-symbol internal-unit var &rest unit-specs)
"macro that creates an object framework that accepts a number and converts to a boxed object and vice versa.  var is used in all functions.  unit pec is of the for (unit-symbol entry-function exit-function).  Example (defunitclass time-t seconds x (minutes (* x 60) (/ x 60)) (hours (* x 3600)(/ x 3600)) (days (* x 24 3600) (/ x 24 3600))"
  (let ((sym-o (gensym))(sym-s (gensym)))
    `(progn
       (defclass ,type-symbol nil ((,internal-unit :initarg ,(to-keyword internal-unit) :reader ,internal-unit)))
       (defmethod print-object( (,sym-o ,type-symbol) ,sym-s)
	 (format ,sym-s "(~a:~a ~a)" ,(symbol-name type-symbol)(,internal-unit ,sym-o),(symbol-name internal-unit)))
       (defgeneric ,internal-unit(object))
       (defmethod ,internal-unit (,var)
	 (make-instance (quote ,type-symbol) ,(to-keyword internal-unit) ,var))
       ;(defmethod ,internal-unit ((object ,type-symbol))
	; (,internal-unit object))
       ,@(loop for (unit-name entry-function exit-function) in unit-specs collecting
	      `(progn
		(defgeneric ,unit-name (object))
		(defmethod ,unit-name (,var)
		  (make-instance (quote ,type-symbol) ,(to-keyword internal-unit) ,entry-function))
		(defmethod ,unit-name ((object ,type-symbol))
		  (let ((,var (,internal-unit object)))
		    ,exit-function))
		nil)))))

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

(defmacro with-gensyms( ( &rest symbol-names ) &body forms)
  `(let (,@(loop for symbol-name in symbol-names collecting
	       `(,symbol-name (gensym ,(symbol-name symbol-name)))))
     ,@forms))

(defmacro with-once-only((&rest args) &body body)
  (let ((sym-args-o (loop for arg in args collecting (gensym "sym-args-o"))))
    `(let ,(mapcar #'list sym-args-o args) ;stash original params
       (let ,(mapcar (lambda(arg) `(,arg (gensym))) args) ;premeptively write code shadowing original args with gensyms
	 `(let (,,@(mapcar (lambda(arg sym-arg-o) ``(,,arg ,,sym-arg-o)) args sym-args-o)) ;rewrite quotation with a let to old arguments
	    ,,@body)))))

(defmacro for-each-range((control-var upper-bound &optional (lower-bound 0) (step 1)) &body body)
  (with-gensyms (!upper-bound !step) 
    `(let ((,control-var ,lower-bound)(,!upper-bound ,upper-bound)(,!step ,step))
       (tagbody 
	resume-loop
	  (when (< ,control-var ,!upper-bound) 
	    ,@body
	    (incf ,control-var ,!step)
	    (go resume-loop))))))

(defmacro map-range((control-var upper-bound &optional (lower-bound 0) (step 1)) expr)
  (with-gensyms (!upper-bound !step !acc) 
    `(let ((,control-var ,lower-bound)(,!upper-bound ,upper-bound)(,!step ,step)(,!acc ,nil))
       (tagbody 
	resume-loop
	  (when (< ,control-var ,!upper-bound)
	    (push ,expr ,!acc)
	    (incf ,control-var ,!step)
	    (go resume-loop)))
       (reverse ,!acc))))

(defun filled-list(num-elements &optional fill-value)
  (loop for x from 1 to num-elements collecting fill-value))

(defmacro with-full-eval(&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro defeasyclass( name (&rest direct-superclasses) (&rest members) &key (export-all t))
  (let ((sym-o (gensym))(sym-s (gensym)))
    (let ((exports))
      (when export-all (push `(export (list ,@(qmap (member) (list 'quote member) members) ,(list 'quote name))) exports))
      `(progn
	 (defclass ,name (,@direct-superclasses)
	   (,@(loop for member in members collecting `(,member :initarg ,(to-keyword member) :reader ,member))))
	 (defmethod print-object( (,sym-o ,name) ,sym-s) (format ,sym-s ,(apply #'concatenate 'string (loop for member in members collecting (% "{~a:~a}" (symbol-name member) "~a"))) ,@(loop for member in members collecting `(format-repl (,member ,sym-o)))))
	 (defun ,name ,members (make-instance (quote ,name) ,@(apply #'concatenate 'list (loop for member in members collecting `(,(to-keyword member) ,member)))))
	 ,@exports))))

(defmacro define-specials(special-specs &optional print-specials-function)
  `(progn
     ,@(remove-nil
	`(
	  ,@(loop for (name value) in special-specs collecting
		 `(defparameter ,name ,value))
	    ,(when print-specials-function
		   `(defun ,print-specials-function ()
		      ,@(loop for (name value) in special-specs collecting
			     `(format t "~a:~a~%" ,(symbol-name name) ,name)))))))) 
 
(defmacro with-collector((pusher) &body body)
  (with-gensyms (!collector-var !obj)
    `(let ((,!collector-var))
       (macrolet ((,pusher (,!obj)
		    `(push ,,!obj ,',!collector-var)))
	 ,@body
	 (reverse ,!collector-var)))))

(defmacro aif(test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro map-rows(lambda-list function list)
  (with-gensyms (lambda-spec)
    `(qmap (,lambda-spec) 
	   (destructuring-bind ,lambda-list ,lambda-spec 
	     ,function)
	   ,list)))

(defmacro with-regexes(regex-specs &body body)
  `(let
       ,(map-rows (var-name regex) `(,var-name (ppcre::create-scanner ,regex)) regex-specs)
     ,@body))

(with-full-eval
  (defun get-first-atom(v)
    (if (atom v)
	v
	(get-first-atom (car v)))))

(defmacro with-regex-matches(regex string match-vars &body body)
  `(aif (~ ,regex ,string)
	(let ,(qmap (var num) 
		    `(,(get-first-atom var)
		      ,(if (atom var)
			   `(funcall it ,num)
			   (destructuring-bind (var place) var
			     (declare (ignore var))
			     `(funcall it ,place))))
		    match-vars (range (1+ (length match-vars)) 1))  
	  ,@body)))

(defmacro with-rebindings(vars lambda-expr &body body)
  `(let ,(qmap (var) `(,var (funcall ,lambda-expr ,var)) vars)
     ,@body))
