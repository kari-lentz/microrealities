(defpackage :utility
  (:documentation "A for general purpose functions and macros")
  (:use :cl)
  (:export :%
	   :string-to-list
	   :range
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
	   :vmap
	   :zip-hash
	   :format-sql
	   :str-to-ubyte
	   :ubyte-to-str
	   :connect-local
	   :connect-remote
	   :disconnect-local
	   :disconnect-remote
	   :query-remote
	   :query-local
	   :lists
	   :vectors
	   :with-local-db-conn
	   :with-remote-db-conn
	   :with-databases
	   :make-nil-str
	   :*remote-host*
	   :*remote-user-id*
	   :*remote-password*
	   :*remote-database*
	   :*local-host*
	   :*local-user-id*
	   :*local-password*
	   :*local-database*))

(in-package :utility)

(defparameter *remote-host* nil)
(defparameter *remote-user-id* nil)
(defparameter *remote-password* nil)
(defparameter *remote-database* nil)

(defparameter *local-host* nil)
(defparameter *local-user-id* nil)
(defparameter *local-password* nil)
(defparameter *local-database* nil)

(defmacro %(fmt-str &rest args)
  (let ((ret (gensym)))
    `(with-output-to-string (,ret) (format (,@ret) ,fmt-str ,@args))))
(defstruct dts ut)

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
  (if (eq seq nil)
      t
      (and (funcall (car seq)) (chain-and (cdr seq)))))

(defun chain-or(seq)
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
  (lambda()(incf init delta)))

(defun format-sql( arg )
    (cond ((not arg) "null")
	  ((integerp arg) (% "~a" arg))
	  ((stringp arg) (% "'~a'" arg))
	  ((dts-p arg) (multiple-value-bind (second minute hour date month year)
			   (decode-universal-time (dts-ut arg)) (% "'~a/~a/~a ~a:~a:~a" year month date hour minute second)))
	  (t (string arg))))

(defmacro with-stdout-null( &body frm )
  `(with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :append) ,@frm))
	  
(defun str-to-ubyte( str )
  (map '(vector (unsigned-byte 8)) (lambda(c) (char-code c)) str))

(defun ubyte-to-str( bytes )
  (map 'string (lambda(byt)(code-char byt)) bytes))

(defun connect-local( &optional (host *local-host*) (database *local-database*) ) 
   (with-stdout-null
     (let ((ret (mssql:connect database *local-user-id* *local-password* host :external-format :latin-1)))
       ret)))

(defun disconnect-local( &optional (conn nil))
  (when conn (progn
	       (mssql:disconnect conn))))

(defun query-local( sql &key (out-format 'lists))
  (let ((rows (with-stdout-null (mssql:query sql :connection mssql:*database*))))
    (cond ((eq out-format 'vectors) (loop for row in rows collecting (vmap row)))
	  (t rows))))
	  
(defmacro with-local-db-conn( &body frms )
  (let ((sym-ret (gensym)))
    `(progn
       (let ((mssql:*database* (connect-local)))
	 (let ((,sym-ret (unwind-protect (progn ,@frms)
			   (disconnect-local mssql:*database*))))
	   ,sym-ret)))))

(defun connect-remote( &optional (host *remote-host*) (database *remote-database*) )
  (let ((ret (cl-mysql:connect :host host :user *remote-user-id* :password *remote-password* :database database)))
       (cl-mysql:query "set names 'utf8'")
       ret))

(defun disconnect-remote( &optional (conn nil))
  (if conn (cl-mysql:disconnect conn) (cl-mysql:disconnect)))

(defun query-remote( sql &key (out-format 'lists))
  (let ((rows (car (car (cl-mysql:query sql))))) 
    (cond ((equal out-format 'vectors) (loop for row in rows collecting (vmap row)))
	  (t rows))))
  
(defmacro with-remote-db-conn(&body frms )
  (let ((sym-ret (gensym)) (sym-conn (gensym)))
    `(let ((,sym-conn (connect-remote)))
       (let ((,sym-ret (unwind-protect (progn ,@frms)
	 (disconnect-remote ,sym-conn))))
	 ,sym-ret))))

(defmacro with-databases(&body frms)
  `(with-remote-db-conn
     (with-local-db-conn ,@frms)))

(defun make-nil-str( str )
  (if (and (stringp str) (> (length str) 0)) 
      str
      nil))

(defun zip-hash( seq-1 seq-2 )
  (let ((ht (make-hash-table)))
    (loop for (x y) in (mapcar (lambda(x y) (list x y)) seq-1 seq-2) do (setf (gethash x ht) y))
    ht))

