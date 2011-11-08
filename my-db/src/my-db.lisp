(defpackage :my-db
  (:documentation "A for general purpose functions and macros")
  (:use :cl :my-env :utility)
  (:export :format-sql
	   :connect-local
	   :connect-remote
	   :disconnect-local
	   :disconnect-remote
	   :query-remote
	   :query-local
	   :with-local-db
	   :with-remote-db
	   :with-databases
	   :make-db-field
	   :make-db-fields
	   :make-db-fields-no-nil
	   :make-db-update
	   :make-db-delete
	   :make-db-insert
	   :mssql-date-error
	   :read-mssql-date
	   :mysql-date-error
	   :read-mysql-date
	   ))

(in-package :my-db)

(defmacro with-stdout-null( &body frm )
  `(with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :append) ,@frm))

(defparameter *sql-months* ({} ("jan" 1) ("feb" 2) ("mar" 3) ("apr" 4) ("may" 5) ("jun" 6) ("jul" 7) ("aug" 8) ("sep" 9) ("oct" 10) ("nov" 11) ("dec" 12)))

(defparameter *mssql-date-scanner*  (ppcre:create-scanner "^([a-zA-Z]+)[\\s]+([0-9]{1,2})[\\s]+([0-9]{2,4})[\\s]+([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2}).*([aApP][mM])$"))

(defun format-sql( arg )
    (cond ((not arg) "null")
	  ((integerp arg) (% "~a" arg))
	  ((stringp arg) (% "'~a'" (ppcre:regex-replace-all "'" arg "''")))
	  ((dts-p arg) (multiple-value-bind (second minute hour date month year)
			   (decode-universal-time (dts-ut arg)) (% "'~a/~a/~a ~a:~a:~a'" year month date hour minute second)))
	  (t (string arg))))

(defun connect-local( &optional (host *local-host*) (user-id *local-user-id*) (password *local-password*) (database *local-database*) ) 
   (with-stdout-null
     (let ((ret (mssql:connect database user-id password host :external-format :latin-1)))
       ret)))

(defun disconnect-local( &optional (conn nil))
  (when conn (progn
	       (mssql:disconnect conn))))

(defun query-local( sql )
  (with-stdout-null (mssql:query sql :connection mssql:*database*)))

(defmacro with-local-db( &body frms )
  (let ((sym-ret (gensym)))
    `(progn
       (let ((mssql:*database* (connect-local)))
	 (let ((,sym-ret (unwind-protect (progn ,@frms)
			   (disconnect-local mssql:*database*))))
	   ,sym-ret)))))

(defun connect-remote( &optional (host *remote-host*) (user-id *remote-user-id*) (password *remote-password*) (database *remote-database*) )
  (let ((ret (cl-mysql:connect :host host :user user-id :password password :database database)))
       (cl-mysql:query "set names 'utf8'")
       ret))

(defun disconnect-remote( &optional (conn nil))
  (if conn (cl-mysql:disconnect conn) (cl-mysql:disconnect)))

(defun query-remote( sql )
  (car (car (cl-mysql:query sql))))

(defmacro with-remote-db( (&key (host nil) (user-id nil) (password nil) (database nil)) &body frms)
  (let ((sym-ret (gensym)) (sym-conn (gensym)))
    (let ((code-block `(let ((,sym-conn (connect-remote )))
			 (let ((,sym-ret (unwind-protect (progn ,@frms)
					   (disconnect-remote ,sym-conn))))
					   ,sym-ret))))

      `(let ,(loop for (sym var) in (zip '(host user-id password database)(list host user-id password database)) when var collecting `(,(.sym '*remote- sym '*) ,var)) (,@code-block)))))

(defmacro with-databases(&body frms)
  `(with-remote-db ()
     (with-local-db ,@frms)))

(defmacro db-equality-set(&rest db-vars) 
  `(join ", "
	 (list ,@(loop for (db-var-col db-var-arg) in
		      (loop for db-var in db-vars collecting (multiple-value-bind (ret) `( ,(% "~a = ~a" (sym-to-db-var db-var) "~a") (format-sql ,db-var)) ret))
		    collecting `(% ,db-var-col ,db-var-arg)))))

(defmacro db-equality-p(&rest db-vars) 
  `(join " and "
	 (list ,@(loop for (db-var-col db-var-arg) in
		      (loop for db-var in db-vars collecting (multiple-value-bind (ret) `( (if ,db-var ,(% "~a = ~a" (sym-to-db-var db-var) "~a") ,(% "~a is ~a" (sym-to-db-var db-var) "~a") )  (format-sql ,db-var)) ret))
		    collecting `(% ,db-var-col ,db-var-arg)))))

(defmacro db-cols(&rest db-cols)
  `(join ", " (list ,@(loop for db-col in db-cols collecting (sym-to-db-var db-col)))))

(defmacro db-vars(&rest db-vars)
  `(join ", " (list ,@(loop for db-var in db-vars collecting `(format-sql ,db-var)))))

(defstruct db-field-t name value)

(defun make-db-field(name value)
  (make-db-field-t :name name :value value))

(defmacro make-db-fields(&rest db-vars)
  `(list ,@(loop for db-var in db-vars collecting `(make-db-field ,(sym-to-db-var db-var) ,db-var))))
  
(defmacro make-db-fields-no-nil(&rest db-vars)
  (let ((sym-field (gensym)))
    `(loop for ,sym-field in (make-db-fields ,@(loop for db-var in db-vars collecting db-var)) when (db-field-t-value ,sym-field) collecting ,sym-field)))

(defun make-db-update(db-table set-vars where-vars)
  (% "update ~a set ~a where ~a"
     db-table
     (join ", " (loop for set-var in set-vars collecting (% "~a = ~a" (db-field-t-name set-var) (format-sql (db-field-t-value set-var)))))
     (join " and " (loop for where-var in where-vars collecting (% "~a ~a ~a" (db-field-t-name where-var) (if (db-field-t-value where-var) "=" "is") (format-sql (db-field-t-value where-var)))))))

(defmacro make-db-update-undocumented-macro(table (&rest set-vars) (&rest where-vars))
  `(% "update ~a set ~a where ~a"
      ,(sym-to-db-var table)
      (db-equality-set ,@(loop for set-var in set-vars collecting set-var))
      (db-equality-p ,@(loop for where-var in where-vars collecting where-var))))

(defmacro make-db-delete(table (&rest where-vars))
  `(% "delete FROM ~a where ~a"
      ,(sym-to-db-var table)
      (db-equality-p ,@(loop for where-var in where-vars collecting where-var))))

(defmacro make-db-insert(table (&rest insert-vars))
  `(% "insert into ~a (~a) values (~a)"
      ,(sym-to-db-var table)
      (db-cols ,@(loop for insert-var in insert-vars collecting insert-var))
      (db-vars ,@(loop for insert-var in insert-vars collecting insert-var))))

(define-condition mssql-date-error 
    (error)
  ((date-str :initarg :str
	:reader mssql-date-error-str)
   (object :initarg :error-object
	:reader mssql-date-error-object))
  (:report (lambda (condition stream)
             (format stream "mssql-date-error parsing: ~a error:  ~a" (mssql-date-error-str condition) (let ((o (mssql-date-error-object condition))) (or o "bad format"))))))

(defun read-mssql-date(date-str &optional (ret-nil-on-error-p t))
  (handler-case
      (flet ((read-year( yr )
	       (+ (parse-integer yr) (if (= (length yr) 2) 2000 0)))
	     (read-month( str )
	       ([nil] *sql-months* (subseq (string-downcase str) 0 3)))
	     (read-hour( hr ampm)
	       (+ (mod (parse-integer hr) 12) (if (equal (string-downcase ampm) "am") 0 12)))
	     (read-item( n )
	       (parse-integer n)))
	
	(when date-str
	  (let ((mo (~ *mssql-date-scanner* date-str)))
	    (unless mo (error (make-condition 'mssql-date-error :str date-str :error-object nil)))
	    (make-dts (read-year (funcall mo 3)) (read-month (funcall mo 1)) (read-item (funcall mo 2)) (read-hour (funcall mo 4) (funcall mo 7)) (read-item (funcall mo 5)) (read-item (funcall mo 6))))))

    (error(error-o) 
      (if ret-nil-on-error-p
	  nil
	  (error (make-condition 'mssql-date-error :str date-str :error-object error-o))))))

(define-condition mysql-date-error (error)
  ((date-int :initarg :int
	:reader mysql-date-error-int)
   (object :initarg :error-object
	:reader mysql-date-error-object))
  (:report (lambda (condition stream)
             (format stream "mysql-date-error parsing: ~a error:  ~a" (mysql-date-error-int condition) (let ((o (mysql-date-error-object condition))) (or o "bad format"))))))

(defun read-mysql-date( date &optional (ret-nil-on-error-p t))
  (handler-case
      (make-dts-from-ut date)
    (error(error-o) 
      (if ret-nil-on-error-p
	  nil
	  (error (make-condition 'mysql-date-error :int date :error-object error-o))))))

(defun read-mysql-date( date &optional (ret-nil-on-error-p t))
  (handler-case
      (make-dts-from-ut date)
    (error(error-o) 
      (if ret-nil-on-error-p
	  nil
	  (error (make-condition 'mysql-date-error :int date :error-object error-o))))))


