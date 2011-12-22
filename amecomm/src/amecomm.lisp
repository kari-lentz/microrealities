(defpackage :amecomm
  (:use :common-lisp :hunchentoot :cl-who :utility )
  (:import-from :my-db :format-sql :query-remote :query-local :read-mssql-date :read-mysql-date)
  (:export run ))

(in-package :amecomm)

(defconstant +debug-logging+ nil)
(defconstant +secret-message+ "secret-message")
(defconstant +done-message+ "i-am-done-my-work")
(defconstant +batch-processor-id+ "batch-processor-port-~a")
(defconstant +batch-script-port+ 6665)
(defconstant +batch-upload-port+ 6666)

(defmacro debug-log-process( control-string &rest format-args )
  (when +debug-logging+
    `(log-process ,control-string ,@format-args)))

(defparameter *wan-upload-host* "https://testcomm.amemusic.com:4430")
(defparameter *wan-download-host* "https://testcomm.amemusic.com:4430")
(defparameter *dialup-upload-host* "http://192.168.1.50")
(defparameter *dialup-download-host* "http://192.168.1.50")
(defparameter *local-upload-host* "https://comm.amemusic.com")
		
(defparameter *hunchentoot-port* 4430)
(defparameter *shutdown-port* 6440)
(defparameter *swank-loader* (merge-pathnames "slime/swank-loader.lisp" my-env:*home-code* ))
(defparameter *swank-port* 4006)
(defparameter *cr+lf* (format nil "~a~a" (code-char 13)(code-char 10)))

(defparameter *raw-latin*
  (flexi-streams:make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defparameter *system-logs* (merge-pathnames "Logs/" my-env:*amecomm-files*))
(ensure-directories-exist *system-logs*)

(defparameter *system-updates* (merge-pathnames "Updates/" my-env:*amecomm-files*))
(ensure-directories-exist *system-updates*)

(defparameter *system-scripts* (merge-pathnames "Scripts/" my-env:*amecomm-files*))
(ensure-directories-exist *system-scripts*)

(defparameter *system-uploads* (merge-pathnames "Uploads/" my-env:*amecomm-files*))
(ensure-directories-exist *system-uploads*)

(defparameter *script-errors* (merge-pathnames "Script-Errors/" my-env:*amecomm-files*))
(ensure-directories-exist *script-errors*)

(defparameter *upload-errors* (merge-pathnames "Upload-Errors/" my-env:*amecomm-files*))
(ensure-directories-exist *upload-errors*)

(defparameter *put-regex-validator* "^/Receiver/Logs/([A-Za-z_0-9]+)/([^\\./][^/]*)\.upl$")
(defparameter *manifest-regex-validator* "^/Updates/Media/([a-zA-Z][a-zA-Z][0-9]+)/Manifest.dat$")

;;; Start the hunchentoot server
(defparameter *log-folder* (merge-pathnames "log/" my-env:*home-amecomm-content*) )
(ensure-directories-exist *log-folder*)

(defparameter *dow-vector* (map 'vector (lambda(x) x) (list "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defparameter *access-log-pathname* (merge-pathnames "access.log" *log-folder* ))
(defparameter *message-log-pathname* (merge-pathnames "error.log" *log-folder* ))
(defparameter *process-log-pathname* (merge-pathnames "process.log" *log-folder* ))

(defparameter *hunchentoot-server* (make-instance 'easy-ssl-acceptor :port *hunchentoot-port* :ssl-certificate-file (merge-pathnames "amecomm/cert/server.crt" my-env:*home-code*)  :ssl-privatekey-file (merge-pathnames "amecomm/cert/server.key" my-env:*home-code*) :ssl-privatekey-password "mizar2020" :access-log-destination *access-log-pathname* :message-log-destination *message-log-pathname*))

(defparameter *max-buffer-size* (* 4 65536))

(defparameter *upload-file-url* nil) 
(defparameter *upload-file-path* nil)
(defparameter *upload-file-hash-string* nil)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun touch(file-path mod-time access-time)
  (let ((unix-mod-time (- (dts-ut mod-time) +unix-epoch+))(unix-access-time (- (dts-ut access-time) +unix-epoch+)))
    (sb-posix:utime file-path unix-access-time unix-mod-time)))

#+(or (and lispworks unix) sbcl clisp)
(defun %stat (filename)
  #+(and lispworks unix) (system:get-file-stat filename)
  #+sbcl (sb-posix:stat filename)
  #+clisp (posix:file-stat filename))

; unix:unix-stat result in CMUCL:
; (values  success    0
;          st-dev     1
;          st-ino     2
;          st-mode    3
;          st-nlink   4
;     	   st-uid     5
;	   st-gid     6
;	   st-rdev    7
;	   st-size    8
;	   st-atime   9
;	   st-mtime   10
;	   st-ctime   11
;	   st-blksize 12
;	   st-blocks) 13

(defun stat-last-access (stat)
  #+(and lispworks unix) (system:file-stat-last-access (%stat stat))
  #+sbcl (sb-posix:stat-atime (%stat stat))
  #+cmu (nth-value 9 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-atime (%stat stat))
  #+openmcl (declare (ignore stat))
  #+openmcl nil)

(defun stat-last-change (stat)
  #+(and lispworks unix) (system:file-stat-last-change (%stat stat))
  #+sbcl (sb-posix:stat-ctime (%stat stat))
  #+cmu (nth-value 11 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-ctime (%stat stat))
  #+openmcl (declare (ignore stat))
  #+openmcl nil)

(defun stat-last-modify (stat)
  #+(and lispworks unix) (system:file-stat-last-modify (%stat stat))
  #+sbcl (sb-posix:stat-mtime (%stat stat))
  #+cmu (nth-value 10 (unix:unix-stat (ext:unix-namestring stat)))
  #+clisp (posix:file-stat-mtime (%stat stat))
  #+openmcl (nth-value 3 (ccl::%stat stat)))

(defun stat (filename)
  (values t
	  (make-dts-from-ut (+ (stat-last-access filename) +unix-epoch+))
	  (make-dts-from-ut (+ (stat-last-change filename) +unix-epoch+))
	  (make-dts-from-ut (+ (stat-last-modify filename) +unix-epoch+))))

(defun try-something(num-attempts function-to-try)
  (loop for attempt from 1 to num-attempts do (when (funcall function-to-try) (return))))

(defmacro with-remote-db( &body frms)
  `(my-db:with-remote-db (:host "127.0.0.1" :user-id "root" :password "topdog" :database "AMECOMM") ,@frms))

(defmacro with-local-db( &body frms)
  `(my-db:with-local-db (:database "AmeMaster") ,@frms))

(defmacro with-local-db-reconnectable(force-reconnect-p &body frms)
  `(my-db:with-local-db (:database "AmeMaster" :force-reconnect-p ,force-reconnect-p) ,@frms))

(defmacro with-html-simple( &body frm)
  `(with-html-output-to-string (*standard-output* nil)
    ,@frm))

(defmacro with-param-something( (var name &optional (type 'string)) &body frm )
  (let ((sym-str-var (gensym)))
    `(let ((,sym-str-var (get-parameter ,name)))
       (if ,sym-str-var
	   (let (
		 (,var
		  ,(cond ((eq type 'integer) `(parse-integer ,sym-str-var))
			 (t sym-str-var))))
	     ,@frm)
	   (progn
	     (log-message* :error "param ~a was null when it needs to be something" ,name)
	     (setf (return-code*) 400)
	     (setf (reply-reason-phrase *reply*) (format nil "~a is null" ,name)))))))

(defmacro with-params-something( (&rest param-specs) &body frm)

  (labels (
	   (nested( tree ) 
	     (if (cdr tree) 
		 (append (car tree) (list (nested (cdr tree)))) 
		 (car tree))))
    
    (nested (concatenate 'list (loop for (var name type) in param-specs collecting `(with-param-something (,var, name ,type))) frm))))

(defmacro with-logged-errors( &body frms )
  `(handler-case
       ,(if (cdr frms)
	    `(progn
	       ,@frms)
	    (car frms))
     (error(err) (progn
		   (setf (return-code*) 500)
		   (send-headers)
		   (log-message* :error "when ~a caught error:~a" (url-decode (script-name *request*)) err)))))
		  
(defmacro with-no-logged-errors( &body frms )
  `(progn ,@frms))

(defmacro with-retries( num-attempts sleep-secs &body frms )
  (let ((sym-attempt (gensym)))
    `(loop for ,sym-attempt from 1 to ,num-attempts do 
	  (handler-case
	      (progn
		,(if (cdr frms)
		     `(progn
			,@frms)
		     (car frms))
		(return nil))
	    (error(err) (progn 
			  (log-message* :error (format nil "when ~a caught error:~a" (script-name *request*) err))
			  (sleep ,sleep-secs)))))))

(defmacro with-threaded-retries( uri num-attempts sleep-secs (&body failover-frms) &body frms )
  (let ((sym-attempt (gensym))(sym-uri (gensym)) (sym-try-block (gensym)))
    `(let ((,sym-uri ,uri))
       (bordeaux-threads:make-thread
	(lambda()
	  (block ,sym-try-block
	    (loop for ,sym-attempt from 1 to ,num-attempts do 
		 (handler-case
		     (progn
		       ,(if (cdr frms)
			    `(progn
			       ,@frms)
			    (car frms))
		     (return-from ,sym-try-block))
		   (error(err) (progn 
			       (log-process (format nil "when synching ~a caught error:~a" ,sym-uri err))
			       (sleep ,sleep-secs)))))
	    ,(if (cdr failover-frms)
		 `(progn
		    ,@failover-frms)
		 (car failover-frms)))) :name ,sym-uri))))

(defun notify-batch-processor(server port &optional (message +secret-message+))
  (let ((socket (usocket:socket-connect server port :protocol :datagram)))
    (unwind-protect
	 (let ((buffer (with-output-to-string (stream) (format stream message)))(length (length message)))
	   (usocket:socket-send socket buffer length)
	   nil)
      (usocket:socket-close socket))))

(defun notify-batch-processor-done (server port)
  (notify-batch-processor server port +done-message+))

(defun notify-batch-script(system-id &rest lines)
  (let ((pf (format nil "~a~a-~8,'0d-~3,'0d.sql" *system-scripts* system-id (get-internal-real-time) (random 1000))))
    (with-open-file (out-stream pf :direction :output)
      (loop for line in lines do (write-line line out-stream)))
    (notify-batch-processor "127.0.0.1" +batch-script-port+)))

(defparameter *process-log-pathname* (merge-pathnames "process.log" *log-folder* ))

(defun log-process( fmt-str &rest args )
  (with-open-file (fo *process-log-pathname* :if-does-not-exist :create :if-exists :append :direction :output)
    (format fo "~a: ~a~%" (make-dts-now) (apply #'% fmt-str args))))

(defun notify-batch-upload(upload-file-path)
  
  (let ((opath (pathname upload-file-path)))
    (rename-file upload-file-path (format nil "~a~a.~a.msg" *system-uploads* (pathname-name opath) (pathname-type opath)))
      (notify-batch-processor "127.0.0.1" +batch-upload-port+)))

;not used anymore so please delete using udp batch processing instead for this
;
(defun run-asynch-script(system-id &rest lines)
  (let ((pf (format nil "~a~a-~8,'0d-~3,'0d.sql" *system-scripts* system-id (get-internal-real-time) (random 1000))))
    (with-open-file (out-stream pf :direction :output)
      (loop for line in lines do (write-line line out-stream)))
    (with-threaded-retries pf 5 5 ()
	   (with-local-db
	     (with-open-file (in-stream pf)
	       (loop for line = (read-line in-stream nil) while line do (query-local line))))
	   (ensure-file-gone pf))))

(defun make-db-script(script &rest format-args)
  (apply #'format nil script (mapcar (lambda(arg) (format-sql arg)) format-args)))
  
(defun ensure-system-table()
  (with-remote-db
    (progn
      (let ((name "REMOTE_AME_SYSTEM"))
	(query-remote (% "create table IF NOT EXISTS ~a (CLIENT_ID int, SYSTEM_ID int, COMPUTER_NAME varchar(32), ZONE_ID int, MAN_EXP_DATE datetime, FORCE_DEACTIVATION varchar(1), CALC_EXP_DATE datetime, SYSTEM_EXPIRES datetime, ACTIVE_CATALOG_VER_NUM int, ACTIVE_PROFILE_VER_NUM int, PRIMARY KEY(client_id, system_id))" name))))))

(defun pattern-exists(folder regex)
  (let ((pf-names (loop for entry in (cl-fad:list-directory folder) collecting (namestring entry))))
    (loop for entry in pf-names when (~ regex entry) collecting entry)))

(defun ensure-basic-system-data(system-id)
  (with-remote-db
    (progn
      (with-local-db 
	(loop for (client-id system-id computer-name zone-id man-exp-date force-deactivation calc-exp-date system-expires) in
	     (query-local "select qe.client_id, qe.system_id, qe.computer_name, l.zone_id, qe.man_exp_date, qe.force_deactivation, qe.calc_exp_date, qe.system_expires from QRY_SYSTEM_EXP_DATES qe inner join rpm_client_location l on qe.location_id = l.location_id where system_id = ~a" system-id)
	   do
	     (progn
	       (query-remote
		(apply #'% "insert into REMOTE_AME_SYSTEM (client_id, system_id, computer_name, zone_id, man_exp_date, force_deactivation, calc_exp_date) values (~a, ~a, ~a, ~a, ~a, ~a, ~a) on duplicate key update computer_name = ~a, zone_id =~a, man_exp_date = ~a, force_deactivation = ~a, calc_exp_date = ~a" (qmap (x) (format-sql x) (list client-id system-id computer-name zone-id (read-mssql-date man-exp-date) force-deactivation (read-mssql-date calc-exp-date) computer-name zone-id (read-mssql-date man-exp-date) force-deactivation (read-mssql-date calc-exp-date)))))))))))

(defun ensure-timezone-table()
  (with-remote-db
      (query-remote "create table IF NOT EXISTS REMOTE_TIME_ZONE (ZONE_ID int, ZONE_NAME varchar(100), ZONE_DESC varchar(100), UTC_BIAS int, DST_BIAS int, LINUX_FILE varchar(256), PRIMARY KEY(ZONE_ID))")))

(defun ensure-timezone-data()
  (with-local-db
    (with-remote-db
      (ensure-timezone-table)
      (loop for (zone-id zone-name zone-desc utc-bias dst-bias linux-file) in
	   (query-local "select zone_id, zone_name, zone_desc, utc_bias, dst_bias, linux_file from TIME_ZONE") 
	 do
	   (progn
	     (query-remote "insert into REMOTE_TIME_ZONE (ZONE_ID, ZONE_NAME, ZONE_DESC, UTC_BIAS, DST_BIAS, LINUX_FILE) values (~a, ~a, ~a, ~a, ~a, ~a) on duplicate key update zone_name = ~a, zone_desc = ~a, utc_bias = ~a, dst_bias = ~a, linux_file = ~a" zone-id zone-name zone-desc utc-bias dst-bias linux-file zone-name zone-desc utc-bias dst-bias linux-file))))))

(defun ensure-update-history-table()
  (with-remote-db
    (query-remote  "create table IF NOT EXISTS REMOTE_RPM_CLIENT_SYSTEM_DISC (CLIENT_ID int, SYSTEM_ID int, DISC_TYPE char(1), RECORDING_TYPE char(1), DISC_SERIAL_NUM int, MEDIA_TYPE varchar(20), DISC_STATUS varchar(20), STATUS_DATE datetime, RELEASED_DATE datetime, PROFILE_ID int, DEPENDENT_DISC_ID varchar(20), PRIORITY int, PRIMARY KEY(system_id, disc_type, recording_type, disc_serial_num))")))
     
(defun ensure-update-history(system-id)
  (with-local-db
    (with-remote-db
      (let ((local-discs))
	; run a replace insert on unique update query
	;
	(loop for (client-id system-id disc-type recording-type disc-serial-num media-type disc-status status-date released-date profile-id dependent-disc-id priority) in 
	     (query-local "select client_id, system_id, disc_type, recording_type, disc_serial_num, media_type, disc_status, status_date, released_date, profile_id, dependent_disc_id, priority from SHORT_UPDATE_HISTORY where system_id = ~a" system-id)
	   do
	     (progn
	       (push (format nil "~a~a~a"  disc-type recording-type disc-serial-num) local-discs) ;accumulate list of local discs 
	       (query-remote "insert into REMOTE_RPM_CLIENT_SYSTEM_DISC (client_id, system_id, disc_type, recording_type, disc_serial_num, media_type, disc_status, status_date, released_date, profile_id, dependent_disc_id, priority) values (~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a) on duplicate key update client_id = ~a, media_type = ~a, disc_status = ~a, status_date = ~a, released_date = ~a, profile_id = ~a, dependent_disc_id = ~a, priority = ~a" client-id system-id disc-type recording-type disc-serial-num media-type disc-status (read-mssql-date status-date) (read-mssql-date released-date) profile-id dependent-disc-id priority client-id media-type disc-status (read-mssql-date status-date) (read-mssql-date released-date) profile-id dependent-disc-id priority)))
	;
	;prune obsolete update images
	;
	(let ((local-disc-clause (if local-discs
				     (format nil " and concat(disc_type, recording_type, disc_serial_num) not in (~a)" (join ", " (qmap (disc) (format-sql disc) local-discs)))
				     "")))

	  (query-remote (format nil "delete from REMOTE_RPM_CLIENT_SYSTEM_DISC where system_id = ~a~a" system-id local-disc-clause)))))))
	 
(defun ensure-system-data(system-id)
  (unless (pattern-exists *system-scripts* (format nil "~a\-.*\.sql$" system-id))
    (handler-case 
	(progn
	  (with-local-db
	    (with-remote-db
	      (ensure-basic-system-data system-id)
	      (ensure-update-history system-id)
	      )))
	(error (err) (log-message* :error "error synching system data:~a" err) nil))))

(defun format-exp-date( dts )
  (% "~a/~a/~a" (dts-month dts) (dts-day dts) (dts-year dts)))

; this is an obsolete URL -- please remove
;	   
(define-easy-handler (server-hosts :uri "/Receiver/GetServerHosts.aspx"
                                :default-request-type :get)
    ()
  (with-logged-errors   
    (with-param-something (system-id "systemid" integer)
	(with-param-something (method "method")
	  (when (= system-id system-id)
	    (with-html-simple
	      (str
	       (apply #'% "<upload>~a</upload>~%<download>~a</download>~%" 
		      (cond ((equalp method "DIALUP") (list *dialup-upload-host* *dialup-download-host*))
			    (t (list *wan-upload-host* *wan-download-host*)))))))))))

(define-easy-handler (time-zone :uri "/Receiver/gettimezone.asp"
                                :default-request-type :get)
    ()
  (with-logged-errors
    (with-param-something (system-id "system_id" integer)
      (with-html-simple
	(ensure-system-data system-id)
	(with-remote-db 
	  (let ((zone-id (car (car (query-remote (format nil "select zone_id from REMOTE_AME_SYSTEM where system_id = ~a" system-id))))))
	    (if zone-id
		(let ((row (car (query-remote (format nil "select ZONE_NAME, UTC_BIAS, DST_BIAS from REMOTE_TIME_ZONE where zone_id = ~a" zone-id)))))
		  (if row 
		      (destructuring-bind (zone-name utc-bias dst-bias) row
			(progn
			  (str (% "Zone Name:~a~aUTC Bias:~a~aDST Bias:~a~a" zone-name *cr+lf* utc-bias *cr+lf* dst-bias *cr+lf*))))  				  
		      (str (% "INVALID TIME ZONE"))))
		(str (% "INVALID SYSTEM ID ~a" system-id)))))))))

(define-easy-handler (time-zone-linux :uri "/Receiver/gettimezone_linux.asp"
                                :default-request-type :get)
    ()
  (with-logged-errors
    (with-param-something (system-id "system_id" integer)
      (with-html-simple
	(ensure-system-data system-id)
	(with-remote-db 
	  (let ((zone-id (car (car (query-remote (format nil "select zone_id from REMOTE_AME_SYSTEM where system_id = ~a" system-id))))))
	    (if zone-id
		(let ((row (car (query-remote (format nil "select linux_file from REMOTE_TIME_ZONE where zone_id = ~a" zone-id)))))
		  (if row 
		      (destructuring-bind (file-name) row
			(progn
			  (str (format nil "File_Name:~a~a" file-name *cr+lf*))))  				  
		      (str (% "INVALID TIME ZONE"))))
		(str (% "INVALID SYSTEM ID ~a" system-id)))))))))


(define-easy-handler (server-time :uri "/Receiver/getservertime.asp"
                                :default-request-type :get)
    ()
  (with-logged-errors
    (with-html-simple
      (str (multiple-value-bind (second minute hour day month year dow dst)(dts-parts (make-dts-now) 0)(declare (ignore dst))(format nil "~a ~a/~a/~a ~2,'0d:~2,'0d:~2,'0d" (aref *dow-vector* dow) month day year hour minute second))))))

(define-easy-handler (expiration-date :uri "/Receiver/getsystemexpirationdate.asp"
                                :default-request-type :get)
    ()

  (with-logged-errors
    (with-param-something (system-id "system_id" integer)
      
      (flet ((pass-record-exp-date-remote( dts )
	       (query-remote (format nil "update REMOTE_AME_SYSTEM set system_expires= ~a where system_id = ~a" (format-sql dts) (format-sql system-id)))
	       dts)
	     
	     (pass-record-exp-date-local( dts )
	       (progn
		 (notify-batch-script system-id
				      (make-db-script (format nil "update RPM_CLIENT_SYSTEM set system_expires= ~a where system_id = ~a" (format-sql dts) (format-sql system-id))))
		 dts)))

	(with-html-simple
	  (with-remote-db
	    (let ((row (car (query-remote (% "select force_deactivation, man_exp_date, calc_exp_date from REMOTE_AME_SYSTEM where system_id = ~a" (format-sql system-id))))))
	      (str (if row
		       (let ((date (destructuring-bind (force-deactivation man-exp-date calc-exp-date) row
				     (cond ((and (~ "[yY]" force-deactivation) man-exp-date) man-exp-date)
					   ((and man-exp-date calc-exp-date (>= man-exp-date calc-exp-date)) man-exp-date)
					   (t calc-exp-date)))))
			 (format-exp-date (if date (pass-record-exp-date-remote (pass-record-exp-date-local (read-mysql-date date))) "INVALID DATE")))
		       (% "INVALID SYSTEM ID ~a" system-id))))))))))

(define-easy-handler (set-current-versions :uri "/Receiver/SetCurrentVersions.aspx"
                                :default-request-type :get)
    ()

  (with-logged-errors
    (with-params-something ((system-id "system" integer) (computer-name "tag" string) (catalog-version "catalog" integer) (profile-version "profile" integer))
      (with-html-simple
	(progn
	  (with-remote-db
	    (query-remote (% "update REMOTE_AME_SYSTEM set active_catalog_ver_num = ~a, active_profile_ver_num = ~a where system_id = ~a and computer_name = ~a" (format-sql catalog-version) (format-sql profile-version) (format-sql system-id) (format-sql computer-name)))
	    (notify-batch-script system-id
			       (make-db-script (format nil "update RPM_CLIENT_SYSTEM set active_catalog_ver_num = ~a, active_profile_ver_num = ~a where system_id = ~a and computer_name = ~a" (format-sql catalog-version) (format-sql profile-version) (format-sql system-id) (format-sql computer-name))))
	    (str "OK")))))))

(define-condition bad-update-status (error)
  ((system-id :initarg :system-id :reader system-id)
   (update-str :initarg :update-str :reader update-str)
   (update-status :initarg :update-status :reader update-status))
  (:report 
   (lambda (err stream)
     (format stream "invalid update status request: system id:~a update:~a status:~a" (system-id err) (update-str err) (update-status err)))))

(define-easy-handler (set-update-status :uri "/Receiver/SetUpdateStatus.aspx"
                                :default-request-type :get)
    ()

  (with-logged-errors 
    (with-params-something ((system-id "system" integer) (update-str "update" string) (media-type "media" string) (status "status" string)(status-date "date" string))
      (with-html-simple
	(let ((status (cond ((equalp status "DONE") "APPLIED") ((equalp (subseq status 0 4) "STEP") "FAILED") (t (error (make-condition 'bad-update-status :system-id system-id :update-id update-str :update-status status))))))
	  (let ((mo (~ "^(AME)?([A-Z])([A-Z])([0-9]+)$" update-str)))
	    (if mo
		(let ((disc-type (funcall mo 2))(recording-type (funcall mo 3)) (disc-serial-num (parse-integer (funcall mo 4))))
		  (if disc-type
		      (let ((status-date (parse-dts-american status-date)))
			(if status-date
			    (progn
			      (with-remote-db
				(query-remote "update REMOTE_RPM_CLIENT_SYSTEM_DISC set disc_status = ~a, media_type = ~a, status_date = ~a where system_id = ~a and disc_type = ~a and recording_type = ~a and disc_serial_num = ~a" status media-type status-date system-id disc-type recording-type disc-serial-num))
			      (notify-batch-script system-id
						 (make-db-script "update RPM_CLIENT_SYSTEM_DISC set disc_status = ~a, media_type = ~a, status_date = ~a where system_id = ~a and disc_type = ~a and recording_type = ~a and disc_serial_num = ~a" status media-type status-date system-id disc-type recording-type disc-serial-num))
			      (str "OK"))
			    (str "Error:  Invalid Date")))
		      (str "ERROR: Invalid Update String"))))))))))
	       
(define-condition invalid-put-request (error) 
  ((uri :initarg :uri
	:reader uri))
  (:report (lambda (condition stream)
             (format stream "invalid uri request: ~a" (uri condition)))))

(defun create-put-dispatcher( regex-validator fhandler )
  (lambda(req)
    (if (equalp (request-method req) :put)
	(let ((uri (url-decode (script-name req))))
	  (if (~ regex-validator uri) 
	      (lambda()
		(funcall fhandler uri  (raw-post-data :want-stream t) req))
	      (log-message* :error "Invalid uri for put request: ~a" uri))))))

(define-condition local-file-missing-t()())

(defun build-hash( fp &optional (memoize-hash-p nil))

  (labels ((calc-hash()
	     (let ((byte-array (make-array *max-buffer-size* :element-type '(unsigned-byte 8))))
	       (apply #'concatenate 'string (map 'list (lambda(x)(% "~2,'0x" x))(ironclad:digest-file :md5 fp :buffer byte-array :start 0 :end *max-buffer-size*))))))

    (if memoize-hash-p
	(let ((fp-hash (format nil "~a.hash" (namestring fp))))
	  (handler-bind
	      ((local-file-missing-t (lambda(err)(declare (ignore err))(invoke-restart 'build-hash-on-the-fly))))
	    (restart-case
		(handler-case
		    (with-open-file (hash-stream-in fp-hash )
		      (let ((hash-str (make-string (file-length hash-stream-in))))
			(read-sequence hash-str hash-stream-in)
			hash-str))
		  (file-error ()(error (make-condition 'local-file-missing-t))))
	      (build-hash-on-the-fly ()
		(let ((hash-string (calc-hash)))
		  (with-open-file (hash-stream-out fp-hash :direction :output :if-does-not-exist :create :if-exists nil)
		    (when hash-stream-out (write-sequence hash-string hash-stream-out)))
		  hash-string)))))
	(calc-hash))))

(define-condition no-upload-directory()())

(defun calculate-upload-hash( upload-file-path )
 
  (let ((byte-array (make-array *max-buffer-size* :element-type '(unsigned-byte 8))))

    (with-open-file (in-stream upload-file-path :element-type '(unsigned-byte 8))
      (setq in-stream (flexi-streams:make-flexi-stream in-stream :external-format *raw-latin*))
      (dotimes (x 3)(read-line in-stream))
      (setf (flexi-streams:flexi-stream-element-type in-stream) '(unsigned-byte 8))
      (apply #'concatenate 'string (map 'list (lambda(x)(% "~2,'0x" x))(ironclad:digest-stream :md5 in-stream :buffer byte-array :start 0 :end *max-buffer-size*))))))


(defun my-put-handler(uri in-stream req)

  (with-logged-errors
  
    (let ((hash-client (header-in "Content-Hash" req)) (mo (~ *put-regex-validator* uri))(local-upload-uri (format nil "~a~a" *local-upload-host* (ppcre:regex-replace-all " " uri "%20")))(file-dts (rfc-1123-date (dts-ut (make-dts-now)))))
      (let ((pf (merge-pathnames (format nil "~a_~a" (funcall mo 1 ) (funcall mo 2)) *system-logs*)))
	  		
	(with-open-file (fo pf :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)

	  (setq fo (flexi-streams:make-flexi-stream fo :external-format *raw-latin*))

	  (write-line local-upload-uri fo) 
	  (write-line hash-client fo) 
	  (write-line file-dts fo)

	  (let ((byte-array (make-array *max-buffer-size* :element-type 'character)))
	    (setf (flexi-streams:flexi-stream-external-format in-stream) *raw-latin*)
	    (tagbody
	     begin-stream
	       (let ((num-bytes (read-sequence byte-array in-stream :end *max-buffer-size*)))
		 (write-sequence byte-array fo :end num-bytes)
		 (when (= num-bytes *max-buffer-size*) (go begin-stream))))))
	    
	(let ((hash-server (calculate-upload-hash pf)))
	  (if (equalp hash-server hash-client)
	      (notify-batch-upload pf)
	      (progn
		(ensure-file-gone pf)
		(log-message* :error "hash mismatch for ~a client-hash:~a server-hash:~a" pf hash-server hash-client) 
		(send-custom-status 500 (format nil "Hash values do not match")))))))))
	    

(define-easy-handler (get-next-update :uri "/Receiver/getnextupdate.asp"
                                :default-request-type :get)
    ()

  (with-logged-errors
    (with-params-something ((system-id "system_id" integer))
      (with-html-simple
	(with-remote-db
	  (let ((next-update (car (query-remote "SELECT csd.disc_type, csd.recording_type, csd.disc_serial_num, csd.dependent_disc_id FROM REMOTE_RPM_CLIENT_SYSTEM_DISC csd WHERE csd.media_type in ('NET','INTERNET') AND csd.disc_status = 'DEPLOYED' AND csd.system_id = ~a AND (csd.dependent_disc_id IS NULL OR EXISTS (SELECT * FROM REMOTE_RPM_CLIENT_SYSTEM_DISC csd2 WHERE csd2.system_id = csd.system_id AND csd2.disc_type = substring(csd.dependent_disc_id, 1, 1) AND csd2.recording_type = substring(csd.dependent_disc_id, 2, 1) AND csd2.disc_serial_num = cast(substring(csd.dependent_disc_id, 3, length(csd.dependent_disc_id) - 2) as signed) AND csd2.disc_status = 'APPLIED')) ORDER BY CASE WHEN priority IS NOT NULL THEN priority ELSE 0 END ASC, released_date ASC LIMIT 1" system-id))))
	  (str 
	   (if next-update 
	       (destructuring-bind (disc-type recording-type disc-serial-num dependent-disc-id) next-update
		 (with-output-to-string (mem-stream)
		    (format mem-stream "UPDATE ID=~a~a~a~a" disc-type recording-type disc-serial-num *cr+lf*)
		    (when dependent-disc-id (format mem-stream "DEPENDENT UPDATE ID=~a~a" dependent-disc-id *cr+lf*))))
	       "NOUPDATESPENDING"))))))))

(defun manifest-handler( update-id )

  (with-logged-errors 
    (handler-bind
	((local-file-missing-t (lambda(err)(declare (ignore err)) (invoke-restart 'get-from-corporate))))
      
      (let ((pf (merge-pathnames (% "~a/Manifest.dat" update-id) *system-updates*)))
	(setf (content-type*)(format nil "text/plain" ))
	
	(restart-case
	    (handler-case
		(with-open-file (fo pf)
		  (let ((manifest-str (make-string (file-length fo))))
		    (read-sequence manifest-str fo)
		    manifest-str))
	      (file-error () (error (make-condition 'local-file-missing-t))))
	  
	  (get-from-corporate () 
	    (multiple-value-bind (manifest-str status-code) (drakma:http-request (% "https://comm.amemusic.com/updates/media/~a/manifest.dat" update-id))
	      (if (= status-code 200)
		  (progn
		    (handler-case 
			(progn
			  (ensure-directories-exist pf)  
			  (with-open-file (fo pf :direction :output :if-exists :supersede)
			    (write-string manifest-str fo)))
		      (error(error-o) (log-message* :error "failed to create manifest for ~a because of error: ~a" update-id error-o)))
		    manifest-str)
		  (setf (return-code*) status-code)))))))))

(defun create-manifest-dispatcher( fhandler )
  (lambda(req)
    (if (equalp (request-method req) :get)
	(let ((uri (url-decode (script-name req))))
	  (let ((mo (~ *manifest-regex-validator* uri)))
	    (when mo
	      (lambda()
		(funcall fhandler (funcall mo 1)))))))))

(defun begin-file-send(content-length file-dts content-hash file-length &optional 304-p)

  (declare (type dts-t file-dts)(type number file-length)(type number content-length))

  (when 304-p 
    (progn
      (setf (return-code*) 304)
      (setf (reply-reason-phrase *reply*) "already downloaded")))
  
  (setf (content-type*) "application/octet-stream")
  (setf	(content-length*) content-length)
  (setf (header-out :last-modified) (rfc-1123-date (dts-ut file-dts)))
  (setf	(header-out :accept-ranges) "bytes")
  (setf (header-out :Content-Hash) content-hash)
  (setf (header-out :File-Length) (format nil "~a" file-length)) 
  (send-headers))

(defun send-custom-status(return-code reason-phrase)
  (setf (return-code*) return-code)
  (setf (reply-reason-phrase *reply*) reason-phrase)
  (send-headers))

(define-condition header-fault-t(error)())
(define-condition content-changed-t(header-fault-t)())
(define-condition already-downloaded-t(header-fault-t)())
(define-condition file-overrun-t(header-fault-t)())

(defun ensure-file-gone( pf )
  (handler-case (delete-file pf)(file-error () nil)))

(defun store-file(pf file-dts seek hash update-id url-path)

  (let ((unverified-file (format nil "~a.unverified" pf))(buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8))))
 
    (if (> seek 0)
	(let ((pf-storage-begin (format nil "~a.begin-part" pf)) (pf-storage-end (format nil "~a.end-part" pf)))
	  (multiple-value-bind (in-stream return-code) 
	      (drakma:http-request "https://comm.amemusic.com/Receiver/DownloadFile2.aspx" :parameters `(("update" . ,update-id) ("path" . ,url-path)) :want-stream t :additional-headers `(("end" . ,seek)))
	    (unwind-protect
		 (when (= return-code 200)
		   (progn
		     (with-open-file (storage-stream pf-storage-begin :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede :if-does-not-exist :create)
		       (tagbody
			begin-stream
			  (let ((bytes-read (read-sequence buffer in-stream)))
			    (write-sequence buffer storage-stream :end bytes-read)
			    (when (= bytes-read *max-buffer-size*) (go begin-stream)))))
		     (merge-files unverified-file pf-storage-begin pf-storage-end)))
	      (progn
		(ensure-file-gone pf-storage-end)
		(ensure-file-gone pf-storage-begin)
		(close in-stream)))))
	(progn
	  (rename-file (format nil "~a.end-part" pf) unverified-file)))
    
    (unwind-protect
	 (let ((str-hash (build-hash unverified-file)))
	   (if (equalp hash str-hash)
	     (progn
	       (touch unverified-file file-dts file-dts)
	       (rename-file unverified-file pf)
	       (dump-string-to-file (format nil "~a.hash" pf) str-hash))
	     (progn
	       (log-message* :error "hash-values do not match for ~a ->deleting" pf))))
	    (ensure-file-gone unverified-file))))
   
(define-easy-handler (download-file-2 :uri "/Receiver/DownloadFile2.aspx"
                                :default-request-type :get)
    ()

  (with-logged-errors 
    (handler-bind 
	((header-fault-t (lambda(err) (declare (ignore err)) (invoke-restart 'send-error-code)))
	 (local-file-missing-t (lambda(err) (declare (ignore err)) (invoke-restart 'get-from-corporate))))
      
      (block begin
	(with-params-something ((update-id "update" string) (path "path" string))
	  (let ((seek (parse-integer-with-default (header-in "seek" *request*) 0))
		(content-hash-in (header-in "Content-Hash" *request*))
		(pf (format nil "~a~a~a" *system-updates* update-id (if (~ "^/" path) path (format nil "/~a" path))))
		(buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8))))
	    
	    (labels 
		
		((bail-if-changed(hash-in hash-out)
		   (restart-case
		       (when (and content-hash-in (not (equalp hash-in hash-out)) (error 'content-changed-t)))
		     (send-error-code()
		       (send-custom-status 308 "content changed")
		       (return-from begin nil))))
		 
		 (bail-if-seek-fault(seek file-length content-hash file-dts)
		   (restart-case
		       (when (= seek file-length) (error 'already-downloaded-t))
		     (send-error-code()
		       (begin-file-send file-length file-dts content-hash file-length t)
		       (return-from begin nil)))
		   (restart-case
		       (when (> seek file-length) (error 'file-overrun-t))
		     (send-stock-error-code()
		       (send-custom-status 400 "file-overrun")
		       (return-from begin nil)))))
	      
	      (restart-case
		  
		  (handler-case
		      (let ((file-dts (nth-value 2 (stat pf)))(content-hash-out (build-hash pf)))
			(bail-if-changed content-hash-in content-hash-out)
			(with-open-file (in-stream pf :element-type '(unsigned-byte 8))
			  (let ((file-length (file-length in-stream)))
			    (bail-if-seek-fault seek file-length content-hash-in file-dts)  
			    (file-position in-stream seek)
			    (let ((content-length (- file-length seek)))
			      (let ((out-stream (begin-file-send content-length file-dts content-hash-out file-length)))
				(tagbody
				 begin-stream
				   (let ((bytes-read (read-sequence buffer in-stream)))
				     (write-sequence buffer out-stream :end bytes-read)
				     (when (= bytes-read *max-buffer-size*) (go begin-stream)))))))))
		    (file-error () (error 'local-file-missing-t))
		    (sb-posix:syscall-error(err) (when (= (sb-posix:syscall-errno err) 2) (error 'local-file-missing-t)) (error err)))
		
		(get-from-corporate()
		  
		  (multiple-value-bind (in-stream return-code headers) 
		      (drakma:http-request "https://comm.amemusic.com/Receiver/DownloadFile2.aspx" :parameters (list (cons "update" update-id) (cons "path" path)) :want-stream t :additional-headers `(("seek" . ,seek)))
		    (unwind-protect
			 (if (= return-code 200)
			     (let ((content-length (parse-integer (cdr (assoc :Content-Length headers))))(file-dts (parse-dts-rfc-1123 (cdr (assoc :Last-Modified headers)))) (content-hash-out (cdr (assoc :Content-Hash headers))) (file-length (parse-integer-with-default (cdr (assoc :File-Length headers)) 0)))
			       (bail-if-changed content-hash-in content-hash-out)
			       (bail-if-seek-fault seek file-length content-hash-in file-dts)
			       (let ((out-stream (begin-file-send content-length file-dts content-hash-out file-length))(pf-storage (format nil "~a.end-part" pf)))
				 (ensure-directories-exist pf-storage)
				 (and
				  (handler-case
				      (with-open-file (storage-stream pf-storage :direction :output :element-type '(unsigned-byte 8) :if-exists nil :if-does-not-exist :create)
					(tagbody
					 begin-stream
					   (let ((bytes-read (read-sequence buffer in-stream)))
					     (write-sequence buffer out-stream :end bytes-read)
					     (when storage-stream (write-sequence buffer storage-stream :end bytes-read))
					     (when (= bytes-read *max-buffer-size*) (go begin-stream))))
					storage-stream)
				    (error ()(ensure-file-gone pf-storage) (send-custom-status 500 "could not stream storage file on proxy"))) 
				  (store-file pf file-dts seek content-hash-out update-id path)))) ;continue to store file if first to open
			   (progn
			     (setf (return-code*) return-code)
			     (send-headers)))
		      (close in-stream))))))))))))
  
(define-easy-handler (set-update-status-delivered :uri "/Receiver/setupdatestatusdelivered.asp"
                                :default-request-type :get)
    ()

  (with-logged-errors
    (with-params-something ((system-id "system_id" integer) (update-str "update_id" string))
      (with-html-simple
	(let ((mo (~ "^([A-Z])([A-Z])([0-9]+)$" update-str)))
	  (if mo
	      (let ((disc-type (funcall mo 1))(recording-type (funcall mo 2)) (disc-serial-num (parse-integer (funcall mo 3))))
		(if disc-type
		    (with-remote-db
		      (progn
			(query-remote "update REMOTE_RPM_CLIENT_SYSTEM_DISC set disc_status = 'DELIVERED', status_date = NOW() where system_id = ~a and disc_type = ~a and recording_type = ~a and disc_serial_num = ~a" system-id disc-type recording-type disc-serial-num)
			(notify-batch-script system-id (make-db-script "update RPM_CLIENT_SYSTEM_DISC set disc_status = 'DELIVERED', status_date = getdate() where system_id = ~a and disc_type = ~a and recording_type = ~a and disc_serial_num = ~a" system-id disc-type recording-type disc-serial-num))
			
			(str "OK")))
		    (str "ERROR: Invalid Update String")))))))))
  
(setf *dispatch-table*
      (list 
       (create-put-dispatcher *put-regex-validator* #'my-put-handler) 
       (create-manifest-dispatcher #'manifest-handler)
       #'dispatch-easy-handlers
       (create-folder-dispatcher-and-handler "/Receiver/" my-env:*home-amecomm-content*)))

(defconstant +script-regex+ "\\.sql$")
(defconstant +was-data+ 1)

(defun batch-script()
  (handler-case
      (handler-bind 
	  ((my-db:db-connect-error (lambda(err)
				     (let ((sleep-secs 60))
				       (log-process "running batch script and caught: ~awill sleep ~a secs and then reconnect" err sleep-secs) 
				       (sleep sleep-secs)
				       (invoke-restart 'reconnect))))
	   (my-db:sql-error (lambda(err)
			      (log-process "running batch script and caught: ~a see the Script-Error directory for a dump of the script" err)(invoke-restart 'send-file-to-error-queue))))   
	
	(let ((script-files (loop for script-file in (fad:list-directory (format nil "~a" *system-scripts*)) 
			       when (~ +script-regex+ (namestring script-file)) 
			       collecting script-file)))
	  (if script-files
	    (let ((force-db-reconnect nil))
	      (tagbody start-script
		 (restart-case 
		     (with-local-db-reconnectable force-db-reconnect
		       (loop for script-file in script-files do
			    (restart-case
				(progn
				  (with-open-file (in-stream script-file)
				    (loop for line = (read-line in-stream nil) while line do 
					 (query-local line)))
				  (ensure-file-gone script-file))
			      (send-file-to-error-queue()
				(let ((po (pathname script-file)))			      
				  (rename-file script-file (format nil "~a~a.err" *script-errors* (pathname-name po))))))))
	       (reconnect()
		 (setf force-db-reconnect t)
		 (go start-script))))
	      +was-data+)
	    nil)))
    (error(err)(log-process "unexpected error ~a during batch script processing" err))))

(define-condition no-upload-message-file(error)())

(define-condition http-error(error)
  ((uri :initform nil :initarg :uri :reader uri)
   (status-code :initarg :status-code :reader status-code)
   (err-reason-phrase :initarg :reason-phrase :reader err-reason-phrase)
   (http-stream :initarg :stream :reader http-stream)
   (must-close-p :initarg :must-close-p :reader must-close-p))
  (:report (lambda(err stream) (format stream "~a ~a ~a" (or (uri err) "unknown url") (status-code err) (err-reason-phrase err)))))

(define-condition server-busy-error(http-error)
  ((status-code :initform 503 :initarg :status-code :reader status-code)
   (err-reason-phrase :initform "Server Busy" :initarg :reason-phrase :reader err-reason-phrase)))

(defgeneric basic-http (url))

(defun throw-if-http-error(status-code reason-phrase uri)
  (unless (= status-code 200)
    (cond ((= status-code 503)(error (make-condition 'server-busy-error :uri uri :reason-phrase reason-phrase)))
	  (t (error (make-condition 'http-error :uri uri :status-code status-code :reason-phrase reason-phrase))))))

(defmethod basic-http((closure function))
  (multiple-value-bind (content status-code headers uri stream must-close-p reason-phrase) (funcall closure)
    (throw-if-http-error status-code reason-phrase uri)
    (values content status-code headers must-close-p stream)))

(defmethod basic-http( url )
  (basic-http (lambda()(drakma:http-request url))))

(define-condition http-connection-failed(error)
  ((url :initarg :url :reader url))
  (:report (lambda(err stream)(format stream "persistent http connect attempt failed for ~a after getting a return of 200"
 (url err)))))

(defmacro with-http-connection((url http-stream-var) &body frms)
  
  (let ((sym-content (gensym))(sym-status-code (gensym)) (sym-headers (gensym)) (sym-uri (gensym)) (sym-must-close-p (gensym)) (sym-reason-phrase(gensym)))
    `(multiple-value-bind (,sym-content ,sym-status-code ,sym-headers ,sym-uri ,http-stream-var ,sym-must-close-p ,sym-reason-phrase)
	 (drakma:http-request ,url :close nil)
       (declare (ignore ,sym-content ,sym-headers))
       (unwind-protect
	    (progn
	      (throw-if-http-error ,sym-status-code ,sym-reason-phrase ,sym-uri)
	      (when ,sym-must-close-p (error (make-condition 'http-connection-failed :url ,url)))
	      (progn
		,@frms))
	 (close ,http-stream-var)))))
  
(defun batch-upload-with-data(upload-messages http-stream)

  (handler-bind
      ((http-error (lambda(err) 
		     (log-process "caught http error during upload: ~a... will retry with new hash file" err)
		     (invoke-restart 'retry-with-new-hash)))
       (server-busy-error (lambda(err)
			    (declare (ignore err))
			    (log-process "caught server busy error so will just sleep and bail")
			    (sleep 5)
			    (return-from batch-upload-with-data))))
    
    (flet ((get-upload-params(in-stream)
	     (values (read-line in-stream) (read-line in-stream) (read-line in-stream)))
	   
	   (skip-past-upload-params(in-stream)
	     (dotimes (n 3) (read-line in-stream)))
	   
	   (run-upload(in-stream http-stream upload-url hash-str dts-str) 
	     (setf (flexi-streams:flexi-stream-element-type in-stream) '(unsigned-byte 8))
	     (basic-http (lambda()(drakma:http-request upload-url :method :put :content in-stream :additional-headers `(("Content-Hash" . ,hash-str) ("Last-Modified" . ,dts-str)) :stream http-stream :close (if http-stream nil t)))))
	   
	   (store-error(upload-file-path)
	     (let ((opath (pathname upload-file-path)))
	       (rename-file upload-file-path (format nil "~a~a.~a" *upload-errors* (pathname-name opath) (pathname-type opath))))))
      
      
      ;; send a basic test query to see if server ok
      ;; then use persistent connection to upload everything
      ;;
      
      (loop for upload-file-path in upload-messages do 
	   
	   (with-open-file (in-stream upload-file-path :element-type '(unsigned-byte 8))
	     (setq in-stream (flexi-streams:make-flexi-stream in-stream :external-format *raw-latin*))
	     (multiple-value-bind (upload-uri hash-str dts-str)(get-upload-params in-stream)		  
	       (restart-case
		   (run-upload in-stream http-stream upload-uri hash-str dts-str)
		 (retry-with-new-hash()
		   (handler-case
		       (let ((new-hash-str (calculate-upload-hash upload-file-path)))
			 (with-open-file (in-stream upload-file-path :if-does-not-exist nil :element-type '(unsigned-byte 8))						
			   (setq in-stream (flexi-streams:make-flexi-stream in-stream :external-format *raw-latin*))
			   (skip-past-upload-params in-stream)
			   (run-upload in-stream nil upload-uri new-hash-str dts-str))
			 (ensure-file-gone upload-file-path))
		     (error(err)
		       (log-process "after retrying still caught: ~a so will file away" err)
		       (store-error upload-file-path)))
		   ;assume orrignal connection was close and bail
		   (return-from batch-upload-with-data +was-data+)))))
	   
	   (ensure-file-gone upload-file-path)))
    +was-data+))
      
(defun batch-upload()

  (handler-case 
      (let ((upload-messages (fad:list-directory (format nil "~a" *system-uploads*))))
	(and upload-messages 
	     (with-http-connection (*local-upload-host* stream)
	       (batch-upload-with-data upload-messages stream))))
    (error(err)
      (progn
	(log-process "batch upload attempt returned error ~a" err)
	nil))))
	     
(defun start-batch-processor(server port batch-process-function)
  (let ((logger *standard-output*))
    (bordeaux-threads:make-thread
     (lambda()
       ;; Open connection
       (let ((socket (usocket:socket-connect nil nil :protocol :datagram :local-host server :local-port port))(last-ret nil))
	 (unwind-protect
	      (progn
		(setf (sb-bsd-sockets:sockopt-reuse-address (usocket:socket socket)) t)
		(format logger "created reusable batch processor server socket:~a" socket)
		(format logger "created batch processor~%" )
		(loop
		   (multiple-value-bind (socket-ret remaining)(usocket:wait-for-input socket :timeout (or last-ret 60) :ready-only t)
		     (declare (ignore remaining))
		     (if socket-ret
			 (progn
			   (debug-log-process "will fetch data for port ~a~%" port)
			   
			   (multiple-value-bind (return-buffer return-length remote-host remove-port)
			       (usocket:socket-receive socket nil 256 :element-type 'character)
			     (declare (ignore remote-host remove-port))
			     (let ((message (subseq return-buffer 0 return-length)))
			       (debug-log-process "received ~a bytes~%data:~a" return-length message)
			       (when (equalp +secret-message+ message)
				 (progn
				   (debug-log-process "will take action~%")
				   (setf last-ret (funcall batch-process-function))))
			       (when (equalp +done-message+ message)
				 (progn
				   (format logger "done all processing on port ~a~%" port)
				   (return))))))
			 (progn
			   (debug-log-process "timed out for port:~a using last-ret:~a" port last-ret)
			   (setf last-ret (funcall batch-process-function)))))))
	   (progn
	     (format logger  "closing socket for port ~a~%" port)
	     (usocket:socket-close socket)))))
     :name (format nil +batch-processor-id+ port))))

(defun kill-batch-processor(thread)
  (format *standard-output* "will kill:~a~%" thread)
  (bordeaux-threads:destroy-thread thread))

(defun kill-all-batch-processors()

  (let ((threads (loop for thread in (bordeaux-threads:all-threads) when (~ "batch-processor" (bordeaux-threads:thread-name thread)) collecting thread)))

    (notify-batch-processor-done "127.0.0.1" 6665)
    (notify-batch-processor-done "127.0.0.1" 6666)

    (loop for thread in threads do (bordeaux-threads:join-thread thread))))

(defun start-swank()

;;; Start swank
  (swank-loader:init)
  (swank:create-server :port *swank-port* :dont-close t)
  (princ "Loaded Swank on port ")
  (princ *swank-port*)(terpri)

;;; Wait and listen for shutdown command
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			     :type :stream :protocol :tcp)))
    
    ;; Listen on a local port for a TCP connection
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) *shutdown-port*)
    (sb-bsd-sockets:socket-listen socket 1)

    ;; When it comes, close the sockets and continue
    (multiple-value-bind (client-socket addr port)
	(sb-bsd-sockets:socket-accept socket)
      (sb-bsd-sockets:socket-close client-socket)
      (sb-bsd-sockets:socket-close socket)
      (and addr addr)
      (and port port))))

(defun stop-swank()

  (format *standard-output* "stopping swank all remaining threads~%")
  
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:quit))

(defun run()

  (handler-bind
      ((my-db:sql-error (lambda(err)
			(progn
			  (log-process "when ensure schema immediately caught ~a but will continue anyways" err)
			  (invoke-restart 'ignore-and-hope)))))
    (restart-case
	(progn
	  (ensure-system-table)
	  (ensure-timezone-data)
	  (ensure-update-history-table))
      (ignore-and-hope())))

  (start *hunchentoot-server*)
  (format t "Hunchentoot server started on port ~a~%" *hunchentoot-port*)

  (start-batch-processor "127.0.0.1" 6665 #'batch-script)
  (start-batch-processor "127.0.0.1" 6666 #'batch-upload)
  (format t "started-batch processor ~%")

  (start-swank)
 
  ;; Shut down Hunchentoot
  (format t "Stopping Hunchentoot...port ~a~%" *hunchentoot-port*)
  (princ "Stopping Hunchentoot...")(terpri)
  (when *hunchentoot-server* (hunchentoot:stop *hunchentoot-server*))
  (kill-all-batch-processors)
  (stop-swank))

(defun test-query()
  (restart-case 
      (with-local-db (query-local "select system_id, computer_name from RPM_CLIENT_SYSTEM where system_id = 60001"))
    (reconnect()
      (my-db:with-local-db (:force-reconnect-p t) (test-query)))
    (just-retry()
      (test-query))))
