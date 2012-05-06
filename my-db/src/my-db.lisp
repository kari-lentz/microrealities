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
	   :pull-conn-spec-local
	   :pull-conn-spec-remote
	   :with-local-db
	   :with-local-transaction
	   :with-remote-db
	   :with-databases
	   :make-db-field
	   :make-db-fields
	   :make-db-fields-no-nil
	   :make-db-update
	   :make-db-delete-fn
	   :make-db-delete
	   :make-db-insert
	   :make-db-insert-fn
	   :db-connect-error
	   :sql-error
	   :mssql-date-error
	   :read-mssql-date
	   :mysql-date-error
	   :read-mysql-date
	   :run-script-remote
	   :run-script-local
	   :vectors
	   :lists
	   ))

(in-package :my-db)

(defmacro with-stdout-null( &body frm )
  `(with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :append) 
     (with-open-file (*error-output* "/dev/null" :direction :output :if-exists :append) ,@frm)))

(defparameter *mysql-db-conn* nil)

(defparameter *sql-months* ({} ("jan" 1) ("feb" 2) ("mar" 3) ("apr" 4) ("may" 5) ("jun" 6) ("jul" 7) ("aug" 8) ("sep" 9) ("oct" 10) ("nov" 11) ("dec" 12)))

(defparameter *mssql-date-scanner*  (ppcre:create-scanner "^([a-zA-Z]+)[\\s]+([0-9]{1,2})[\\s]+([0-9]{2,4})[\\s]+([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2}).*([aApP][mM])$"))

;(defun format-sql( arg )
;    (cond ((not arg) "null")
;	  ((integerp arg) (% "~a" arg))
;	  ((stringp arg) (% "'~a'" (ppcre:regex-replace-all "'" arg "''")))
;	  ((dts-p arg) (multiple-value-bind (second minute hour date month year)
;			   (decode-universal-time (dts-ut arg)) (% "'~a/~a/~a ~a:~a:~a'" year month date hour minute second)))
;	  ((floatp arg) (% "~$" arg))
;	  ((pathnamep arg) (% "'~a'" (ppcre:regex-replace-all "'" (namestring arg) "''" )))
;	  (t (string arg))))

(defgeneric format-sql( arg ))  

(defmethod format-sql( arg )
  (string arg))

(defmethod format-sql( (arg null) )
  "null")

(defmethod format-sql( (arg integer) )
  (% "~a" arg))

(defmethod format-sql( (arg string) )
  (% "'~a'" (ppcre:regex-replace-all "'" arg "''")))

(defmethod format-sql( (arg dts-t) )
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (dts-ut arg)) (% "'~a/~a/~a ~a:~a:~a'" year month date hour minute second)))

(defmethod format-sql( (arg float) )
  (% "~$" arg))

(defmethod format-sql( (arg pathname) )
  (% "'~a'" (ppcre:regex-replace-all "'" (namestring arg) "''" )))

(define-condition sql-error 
    (error)
  ((sql :initarg :sql
	:reader sql)
   (error-object :initarg :error-object
	:reader error-object))
  (:report (lambda (condition stream)
             (format stream "sql error->:~%sql:~a~%error:~a~%" (sql condition) (error-object condition)))))

(define-condition mssql-error(sql-error)())
(define-condition mysql-error(sql-error)())

(define-condition db-connect-error (sql-error)
  ((connection-spec :initform nil :initarg :connection-spec :reader connection-spec)
   (error-object :initarg :error-object :reader error-object))
  (:report (lambda( err stream) (format stream "error making database connection using spec:~a~%error:~a~%" (or (connection-spec err) "N/A") (error-object err)))))  

(defun connect-local( &optional (host *local-host*) (user-id *local-user-id*) (password *local-password*) (database *local-database*) ) 
  (handler-case
      (with-stdout-null
	(mssql:connect database user-id password host :external-format :latin-1))
    (error (err) (error (make-condition 'db-connect-error :connection-spec (format nil "~a:~a:~a:~a" host user-id password database) :error-object err)))))

(defun disconnect-local( &optional (conn nil))
  (when conn (progn
	       (mssql:disconnect conn))))

(defun query-local( sql &rest fmt-args)
  (let ((sql (apply #'% sql (mapcar (lambda(x)(format-sql x)) fmt-args))))
    (handler-case
	(with-stdout-null (mssql:query sql :connection mssql:*database*))
      (error(error-o) (error (make-condition 'mssql-error :sql sql :error-object error-o)))))) 

(defun pull-conn-spec-local()
  (list *local-host* *local-user-id* *local-password* *local-database*))

(defmacro with-local-db( (&key (host nil) (user-id nil) (password nil) (database nil) force-reconnect-p) &body frms)
  (let ((sym-old-conn-spec (gensym))(sym-frms (gensym)))
    (let ((code-block `(let ((mssql:*database* (connect-local)))
			 (unwind-protect (funcall ,sym-frms)
			   (disconnect-local mssql:*database*)))))

      `(let ((,sym-old-conn-spec (pull-conn-spec-local))(,sym-frms (lambda() (progn ,@frms))))
	 (let ,(loop for (sym var) in (zip '(host user-id password database)(list host user-id password database)) when var collecting `(,(to-qualified-symbol 'my-db (.sym '*local- sym '*)) ,var)) 
	   (if (and (not ,force-reconnect-p) mssql:*database* (equalp ,sym-old-conn-spec (pull-conn-spec-local)))
	       (funcall ,sym-frms)
	       (,@code-block)))))))

(defmacro with-local-transaction( &body frms )
  `(with-local-db ()
	 (mssql:with-transaction (:connection mssql:*database*) ,@frms)))
      
(defun connect-remote( &optional (host *remote-host*) (user-id *remote-user-id*) (password *remote-password*) (database *remote-database*) )
  (let ((ret (cl-mysql:connect :host host :user user-id :password password :database database)))
    (cl-mysql:query "set names 'utf8'" :database ret)
    ret))

(defun disconnect-remote( &optional (conn nil))
  (if conn 
      (cl-mysql:disconnect conn)
      (cl-mysql:disconnect)))

(defun query-remote( sql &rest fmt-args)
  (let ((sql (apply #'% sql (mapcar (lambda(x) (format-sql x)) fmt-args))))
    (handler-case
	(car (car (cl-mysql:query sql :database *mysql-db-conn*)))
      (error(error-o) (error (make-condition 'mysql-error :sql sql :error-object error-o))))))

(defun pull-conn-spec-remote()
  (list *remote-host* *remote-user-id* *remote-password* *remote-database*))

(defmacro with-remote-db( (&key (host nil) (user-id nil) (password nil) (database nil) force-reconnect-p) &body frms)
  (let ((sym-old-conn-spec (gensym))(sym-frms (gensym)))
    (let ((code-block `(let ((*mysql-db-conn* (connect-remote)))
			 (unwind-protect (funcall ,sym-frms)
					   (disconnect-remote *mysql-db-conn*)))))

      `(let ((,sym-old-conn-spec (pull-conn-spec-remote))(,sym-frms (lambda() (progn ,@frms))))
	 (let ,(loop for (sym var) in (zip '(host user-id password database)(list host user-id password database)) when var collecting `(,(to-qualified-symbol 'my-db (.sym '*remote- sym '*)) ,var)) 
	   (if (and (not ,force-reconnect-p) *mysql-db-conn* (equalp ,sym-old-conn-spec (pull-conn-spec-remote)))
	       (funcall ,sym-frms)
	       (,@code-block)))))))



(defmacro with-databases(&body frms)
  `(with-remote-db ()
     (with-local-db () ,@frms)))

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

(defun make-db-delete-fn(table field-objects)
  (% "delete from ~a where ~a"
     table
     (join " and " (loop for field-object in field-objects collecting (% "~a ~a ~a" (db-field-t-name field-object) (if (db-field-t-value field-object) "=" "is") (format-sql (db-field-t-value field-object)))))))

(defmacro make-db-delete(table (&rest where-vars))
  `(% "delete FROM ~a where ~a"
      ,(sym-to-db-var table)
      (db-equality-p ,@(loop for where-var in where-vars collecting where-var))))

(defun make-db-insert-fn(table field-objects)
  (% "insert into ~a (~a) values (~a)"
     table
     (join ", " (loop for ofield in field-objects collecting (db-field-t-name ofield)))
     (join ", " (loop for ofield in field-objects collecting (format-sql (db-field-t-value ofield))))))

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

(defun run-script-remote(&rest lines)
  (with-remote-db ()
    (loop for line in lines
       do
	 (query-remote line))))

(defun run-script-local(&rest lines)
  (with-local-db ()
    (loop for line in lines
       do
	 (query-local line))))

