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
	   :post-fix
	   :debug-print
	   :debug-print-str
	   :with-no-package-lock
	   :make-tree
	   :[]
	   :[nil]
	   :{}
	   :~
	   :join
	   :make-ctr
	   :str-to-ubyte
	   :ubyte-to-str
	   :make-nil-str
	   :vmap
	   :.sym
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
	   :remove-nil
	   :remove-unspeced-params))

(in-package :utility)

(defmacro %(fmt-str &rest args)
  (let ((ret (gensym)))
    `(with-output-to-string (,ret) (format (,@ret) ,fmt-str ,@args))))

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
  
(defun ~( regex str)
  (multiple-value-bind (begin-match end-match begin-groups end-groups) (ppcre:scan regex str)
    (if (and begin-match end-match)
	(let ((mv (map 'vector (lambda(x y) (subseq str x y)) begin-groups end-groups)))
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

(defun make-dts(year month day hour minute second)
  (make-dts-t :ut (encode-universal-time second minute hour day month year)))

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

(defmethod print-object((arg dts-t) s)
  (handler-case
      (multiple-value-bind (second minute hour date month year) (decode-universal-time (dts-t-ut arg)) (format s "~a-~a-~a ~a:~a:~a" year month date hour minute second))
    (error() (format s "bad object"))))

(defun remove-nil( seq )
  (remove-if-not (lambda(x)x) seq))

(defmacro remove-unspeced-params( &rest params )
  `(remove-nil 
    (list ,@(loop for param in params collecting 
		 `(if ,(.sym param '-p) ,param nil)))))

