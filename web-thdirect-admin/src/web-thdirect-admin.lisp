(defpackage :web-thdirect-admin
  (:use :common-lisp :hunchentoot :cl-who :utility :my-db :thdirect-admin)
  (:export run test-cl-who))

(in-package :web-thdirect-admin)

(defparameter *raw-latin*
  (flexi-streams:make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defparameter *hunchentoot-port* 8080)

(defparameter *log-folder* (merge-pathnames "log/" my-env:*thdirect-admin-files*))
(ensure-directories-exist *log-folder*)

(defparameter *access-log-pathname* (merge-pathnames "access.log" *log-folder* ))
(defparameter *message-log-pathname* (merge-pathnames "error.log" *log-folder* ))
(defparameter *process-log-pathname* (merge-pathnames "process.log" *log-folder* ))

;;; create the hunchentoot server object
(defparameter *hunchentoot-server* (make-instance 'easy-acceptor :port *hunchentoot-port* :access-log-destination *access-log-pathname* :message-log-destination *message-log-pathname*))

(defparameter *shutdown-port* 6441)
(defparameter *swank-loader* (merge-pathnames "slime/swank-loader.lisp" my-env:*home-code* ))
(defparameter *swank-port* 4007)

(defparameter *system-scripts* (merge-pathnames "Scripts/" my-env:*thdirect-admin-files*))
(ensure-directories-exist *system-scripts*)

(defparameter *script-errors* (merge-pathnames "Script-Errors/" my-env:*thdirect-admin-files*))
(ensure-directories-exist *script-errors*)

(defconstant +debug-logging+ nil)
(defparameter *secret-message* "secret-message")
(defparameter *done-message* "i-am-done-my-work")
(defparameter *batch-processor-id* "batch-processor-port-~a")
(defparameter *batch-script-port* 6667)

(defun ensure-file-gone( pf )
  (handler-case (delete-file pf)(file-error () nil)))

(defun log-process( fmt-str &rest args )
  (with-open-file (fo *process-log-pathname* :if-does-not-exist :create :if-exists :append :direction :output)
    (format fo "~a: ~a~%" (make-dts-now) (apply #'% fmt-str args))))

(defmacro debug-log-process( control-string &rest format-args )
  (when +debug-logging+
    `(log-process ,control-string ,@format-args)))

(defun notify-batch-processor(server port &optional (message *secret-message*))
  (let ((socket (usocket:socket-connect server port :protocol :datagram)))
    (unwind-protect
	 (let ((buffer (with-output-to-string (stream) (format stream message)))(length (length message)))
	   (usocket:socket-send socket buffer length)
	   nil)
      (usocket:socket-close socket))))

(defun notify-batch-processor-done (server port)
  (notify-batch-processor server port *done-message*))

(defun notify-batch-script(client-id user-id &rest lines)
  (let ((pf (format nil "~a~8,'0d-~3,'0d.sql" *system-scripts* (get-internal-real-time) (random 1000))))
    (with-open-file (out-stream pf :direction :output)
      (write-line (format nil "~a" client-id) out-stream)
      (write-line user-id out-stream)
      (loop for line in lines do (write-line line out-stream)))
    (notify-batch-processor "127.0.0.1" *batch-script-port*)))

(defmacro with-local-db-reconnectable(force-reconnect-p &body frms)
  `(my-db:with-local-db (:database "Web" :force-reconnect-p ,force-reconnect-p) ,@frms))

(setf *dispatch-table*
      (list #'dispatch-easy-handlers
	    (create-folder-dispatcher-and-handler "/account/" my-env:*home-thdirect-content*)
	    (create-folder-dispatcher-and-handler "/reporting/" my-env:*home-thdirect-content*)))

(define-easy-handler (account-manage :uri "/account/manage"
                                :default-request-type :post)
    (
     (modifier :parameter-type 'string)
     (client-id :parameter-type 'integer)
     (user-id :parameter-type 'string)
     (user-password :parameter-type 'string)
     (first-name :parameter-type 'string)
     (last-name :parameter-type 'string)
     (email :parameter-type 'string) 
     (welcome-email-str :parameter-type 'string)
     )
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (progn 
      (setf client-id (or client-id (get-parameter "client-id")))
      (setf user-id (or user-id (get-parameter "user-id")))
      
      (let ((modifier-next (if (or modifier (and client-id user-id)) "edit" "add")))
	(when (and (not modifier) (equal modifier-next "edit"))
	  (with-databases
	    (destructuring-bind (first-name-in last-name-in user-password-in email-in)(car (query-remote (repeat-params (% "select first_name, last_name, user_password, email_address from WEB_USER where client_id = ~a and user_id = ~a") format-sql client-id user-id)))
	      (progn
		(setf first-name first-name-in)
		(setf last-name last-name-in)
		(setf user-password user-password-in)
		(setf email email-in)))))
	(htm 
	 (:html
	  (:head 
	   (:title "Add/Edit user")
	   (:link :rel "stylesheet" :type "text/css" :href "th-styles.css")
	   (:script :language "javascript" :src "accounts.js"))
	  (:body :style "margin: 10px" :class "whole-doc" :onload (% "load_popup(\"~a\");" modifier)
		 (:table :width "100%" :align :center
			 (:tr :width "100%" :align :center
			      (:td (:div :class "heading-text-popup" "Add/Edit User"))(:td (:img :src "th-logo.png"))))
		 (:p (:form
		      :method :post
		      (:table :align :center
			      :border 0 :cellpadding 5 :cellspacing 0
 
			      (:tr (:td :style "text-align: right" (str "Client ID:")) (:td (if client-id (htm (:input :type :text :name "client-id" :value client-id :disabled "disabled")) (htm (:input :type :text :name "client-id" :value client-id)))))
			    
			      (:tr (:td :style "text-align: right" (str "User ID:")) (:td user-id (:input :type :text :name "user-id" :value user-id :disabled "disabled"))) 
			    
			      (:tr (:td :style "text-align: right" (str "Password:")) (:td (if (equal modifier-next "add") (htm (:input :type :text :name "user-password" :value user-password :disabled "disabled"))(htm (:input :type :text :name "user-password" :value user-password)))))
			      
			      (:tr (:td :style "text-align: right" (str "First Name:")) (:td (:input :type :text :name "first-name" :value first-name)))
			    
			      (:tr (:td :style "text-align: right" (str "Last Name:")) (:td (:input :type :text :name "last-name" :value last-name )))
			    
			      (:tr (:td :style "text-align: right" (str "Email:")) (:td (:input :type :text :name "email" :value email)))
			      (:tr (:td (:input :type :hidden :name "modifier" :value (str modifier-next))))
			      
			      (:tr (:td (:input :type :submit :value "Submit") (:td (:button :onclick "cancel_popup();" "Cancel"))))))))))

	(setf (header-out "user-password") user-password)
	(setf (header-out "first-name") first-name)
	(setf (header-out "last-name") last-name)
	(setf (header-out "email") email)
	(setf (header-out "welcome-email-str") welcome-email-str)
       
	(when modifier
	  (with-databases
	    (let ((email-address email))
	      (cond ((equal modifier "add") (add-user client-id first-name last-name :email email-address :user-id (make-nil-str user-id) :user-password (make-nil-str user-password) :welcome-email-str welcome-email-str))
		    ((equal modifier "delete")(delete-user client-id user-id))
		    (t (update-user (make-db-fields-no-nil user-password first-name last-name email-address) client-id user-id))))))))))

(define-easy-handler (account-report :uri "/account/report"
				  :default-request-type :post)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head 
      (:title "Top Hits Direct User Report")
      (:link :rel "stylesheet" :type "text/css" :href "th-styles.css")
      (:script :language "javascript" :src "accounts.js"))
     (:body :class "whole-doc"
      :style "margin: 20px"
      (:table 
       (:tr
	(:td
	 (:table(:tr :class "heading-text-1" 
		 (:td "Top Hits Direct User Report")(:td (:img :src "th-logo.png"))))))
       (:tr
	(:td :align "center"
	 (with-databases
	   (htm
	    (:table :cellpadding 2 :cellspacing 2 :width "100%"
		    (:tr :class "heading-text-2"
			 (loop for col-name in `("Client ID" "First Name" "Last Name" "User ID" "Password" "Email") do (htm (:td (str col-name)))))
		    (:tr(:td :colspan 7 (:hr)))
		    (let ((row-idx 0))
		      (let ((fctr (make-ctr row-idx)))
			(loop for row in (query-local "select client_id, first_name, last_name, user_id, user_password, email_address from WEB_USER") do
			     (destructuring-bind (client-id first-name last-name user-id user-password email-address) row 
			       (htm
				(:tr :class (if (= (mod row-idx 2) 0) "grid-row" "grid-row-alt")
				     (:td (str client-id) )
				     (:td (str first-name) )
				     (:td (str last-name) )
				     (:td (str user-id) )
				     (:td (str user-password))
				     (:td (str email-address))
				     (:td (:a :href "#" :onclick (% "edit_account(\"~a\", \"~a\"); return false;" client-id user-id) "edit")) 
				     )))
			     (setf row-idx (funcall fctr))
			     ))))))))
       (:tr
	(:td :colspan "7" :align "center"
	 (:button :onclick "add_account();" "Add User"))))))))

(define-easy-handler (account-new-user-id :uri "/account/new-user-id"
				  :default-request-type :post)
    ((first-name :parameter-type 'string)
     (last-name :parameter-type 'string)
     (user-ids :parameter-type 'string))
     (with-html-output-to-string (*standard-output* nil :prologue nil) 
      (let ((ret (make-valid-user-id first-name last-name (ppcre:split ":" user-ids))))
	  (str ret))))

(defun log-error( msg &optional (error-o nil) )
  (let ((err-msg 
	 (if error-o 
	     (% "~a ~a" msg error-o)
	     (% "~a" msg))))
    (progn
      (log-message* :error err-msg)
      err-msg)))

(define-easy-handler (account-touch-expiration-date :uri "/account/touch-expiration-date"
                                :default-request-type :post)
    ((client-id :parameter-type 'string)
     (user-id :parameter-type 'string))

  (with-databases
    (update-user nil client-id user-id)
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (str
       (read-mysql-date 
	(car
	 (car
	  (query-remote (% "select EXPIRATION_DATE from WEB_USER where client_id = ~a and user_id = ~a" (format-sql client-id) (format-sql user-id))))))))))

(define-easy-handler (account-touch-expiration-date-remote :uri "/account/touch-expiration-date-remote"
                                :default-request-type :post)
    ((client-id :parameter-type 'string)
     (user-id :parameter-type 'string)
     (expiration-date :parameter-type 'string))
  
  (handler-case
      (with-remote-db ()
	     
	(query-remote (% "update WEB_USER set expiration_date = ~a where client_id = ~a and user_id = ~a" (format-sql expiration-date) (format-sql client-id) (format-sql user-id)))
	
	(with-html-output-to-string (*standard-output* nil :prologue nil)
	  (str
	   (read-mysql-date 
	    (car
	     (car
	      (query-remote (% "select EXPIRATION_DATE from WEB_USER where client_id = ~a and user_id = ~a" (format-sql client-id) (format-sql user-id)))))))))
    (error (error-o) (log-error "error saving expiration date:" error-o)(setf (return-code* *reply*) 500))))

(define-easy-handler (account-touch-all-expiration-dates :uri "/account/touch-all-expiration-dates"
                                :default-request-type :post)
    ()
  (handler-case
      (touch-all-expiration-dates)
    (error (error-o) (log-error "error doing global updating for expiration dates: ~a" error-o)(setf (return-code* *reply*) 500))))
	   
(define-easy-handler (account-welcome :uri "/account/welcome"
                                :default-request-type :post)
    ((user-id :parameter-type 'string)
     (user-password :parameter-type 'string)
     (email-address :parameter-type 'string))

  (handler-case
      (if (and user-id user-password email-address)
	  (progn
	    (send-email-welcome email-address user-id user-password)
	    "SUCCESS")
	  "INCOMPLETE-INFO")
    (email-error(error-o)(log-error "error while ending email" error-o))
    (error(error-o) (log-error "error while ending email" error-o))))
	
(defun get-welcome-stragglers()
  (with-databases
    (loop for (user-id user-password email-address) in (query-local "select user_id, user_password, email_address from WEB_USER where welcome_email is null and email_address like '%@%' and client_id <> 100") do
	 (handler-case
	     (send-email-welcome email-address user-id user-password)
	   (email-error(error-o)(log-error "error while ending email" error-o))
	   (error (error-o) (log-error "error while ending email" error-o))))))
	   

(define-easy-handler (account-welcome-batch :uri "/account/welcome-batch"
                                :default-request-type :get)
    ((verify-auth :parameter-type 'string))
  (when (equal verify-auth "THDIRECTISAGREATPRODUCT")
    (get-welcome-stragglers)))

(define-easy-handler (account-create-and-welcome-batch :uri "/account/create-and-welcome-batch"
                                :default-request-type :get)
    ((verify-auth :parameter-type 'string))
  (progn
    (when (equal verify-auth "THDIRECTISAGREATPRODUCT")
      (batch-create 50))
    (get-welcome-stragglers)))

(define-easy-handler (account-send-promo-pack :uri "/account/send-promo-pack"
                                :default-request-type :post)
    ((user-id :parameter-type 'string)
     (user-password :parameter-type 'string)
     (email-address :parameter-type 'string))

  (handler-case
      (if (and user-id user-password email-address)
	  (progn
	    (send-email-prospect email-address user-id user-password)
	    "SUCCESS")
	  "INCOMPLETE-INFO")
    (email-error(error-o)(log-error "error while ending email" error-o))
    (error(error-o) (log-error "error while ending email" error-o))))



(define-easy-handler (testing-email-html :uri "/testing/email/html"
				  :default-request-type :post)
    ()
  (compose-html-email "TH_KLentz" "roger22"))

(define-easy-handler (testing-email-text :uri "/testing/email/text"
				  :default-request-type :post)
    ()
  (compose-text-email "TH_KLentz" "roger22"))

(defun show-scalar-grid( heading scalar-grid )
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:br)
    (:table :align "center"
     (:tr (:td :class "heading-text-2" (str heading)))
     (let ((fctr (make-ctr 0)))
       (loop for (heading value) in scalar-grid collecting
	    (htm
	     (:tr :class (if (= (mod (funcall fctr) 2) 0) "grid-row" "grid-row-alt")
		  (:td (str heading))(:td (str value)))))))))

(defmacro define-formatted-html-page((sym-unique uri display-title &optional (request-type :post)) &body frms)
  
  `(define-easy-handler (,sym-unique :uri ,uri
                                :default-request-type ,request-type)
       ()
     (with-html-output-to-string (*standard-output* nil :prologue t)
       (:html
	(:head
	 (:title ,display-title)
	 (:link :rel "stylesheet" :type "text/css" :href "th-styles.css"))
	(:body :class "whole-doc"
	       (:table :align "center"
		       (:tr :class "heading-text-1" 
			    (:td ,display-title)(:td (:img :src "th-logo.png"))))
	       ,@frms)))))

(define-formatted-html-page (reporting-aggregates "/reporting/aggregates" "Top Hits Direct Aggregates Report") 
  (destructuring-bind (distinct-users-last-week distinct-users-last-month distinct-users-lifetime downloads-last-week downloads-last-month batch-email-users batch-emails-sent) (aggregates) 
    (str
     (format nil "~a~%~a~%~a~%"
	(show-scalar-grid "Distinct Downloading Users" `(("Last Week" ,distinct-users-last-week) ("Last Month" ,distinct-users-last-month) ("Overall" ,distinct-users-lifetime)))
	(show-scalar-grid "Downloads" `(("Last Week" ,downloads-last-week) ("Last Month" ,downloads-last-month))) 
	(show-scalar-grid "Batch Emails" `(("Downloading Users" ,batch-email-users) ("Users Batch Emailed" ,batch-emails-sent)))))))

(defun show-promo-pack-results(dts-display)
  (let ((dts-begin (dts+ dts-display -3))(dts-end (dts+ dts-display 3)))
    (multiple-value-bind 
	  (total-sent total-logins active-trials subscriptions) 
	(promo-pack-replies dts-begin dts-end)
      (format nil "~a~%"
	      (show-scalar-grid (format nil "~a" (dts-to-Y/M/D dts-display)) `(("Total Sent" ,total-sent) ("Total First Logins" ,total-logins) ("Active Trials" ,active-trials) ("Subscriptions", subscriptions)))))))

(define-formatted-html-page (reporting-promo-pack-emails "/reporting/promo-pack-emails" "Top Hits Direct Promo Pack Emails Report") 
  (with-local-db (:host "192.168.0.24" :database "Billing")
    (str
     (format nil "~a~%~a~%"
	     (show-promo-pack-results (make-dts 2012 4 2 0 0 0))
	     (show-promo-pack-results (make-dts 2012 4 23 0 0 0))))))
       															
(defun begin-file-send(file-length &optional (file-dts (make-dts-now)))
  
  (declare (type dts-t file-dts) (type number file-length))
  
  (setf (content-type*) "application/octet-stream")
  (setf	(content-length*) file-length)
  (setf (header-out :last-modified) (rfc-1123-date (dts-ut file-dts)))
  (setf	(header-out :accept-ranges) "bytes")
  (send-headers))

(define-easy-handler (chart-data :uri "/chart-data-pdf"
                                :default-request-type :get)
    ((disc-type :parameter-type 'string))

  (let ((octets (flexi-streams:with-output-to-sequence (mem-stream)
		  (cond ((equalp "c" disc-type) (th-chart-data:chart-data-c mem-stream))
			(t (th-chart-data:chart-data-t mem-stream))))))
    
    (let ((out-stream (begin-file-send (length octets))))
      (write-sequence octets out-stream))))

(define-easy-handler (update-login-info :uri "/update-login-info"
                                :default-request-type :post)
    ((user-id :parameter-type 'string))

     (with-remote-db ()
       (let ((row (car (query-remote "select client_id, user_password, first_login_dts from WEB_USER where user_id = ~a" user-id))))

	 (if row
	     (destructuring-bind (client-id user-password first-login-dts) row

	       (let ((now (make-dts-now)))

		 (let ((sql (format nil "update WEB_USER set user_password = ~a, first_login_dts = ~a, last_login_dts = ~a where user_id = ~a" (format-sql user-password) (format-sql (or (read-mysql-date first-login-dts) now)) (format-sql now) (format-sql user-id))))
		   
		   (query-remote sql)
		   (notify-batch-script client-id user-id sql))))
	     (log-message* :error "bad login user-id:~a" user-id))))) 


(defconstant +was-data+ 1)
(defparameter *script-regex* "\\.sql$")

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
			       when (~ *script-regex* (namestring script-file)) 
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
				    (let ((client-id (read-line in-stream nil))
					  (user-id (read-line in-stream nil)))
				      (loop for line = (read-line in-stream nil) while line do 
					   (query-local line))
				      (update-user nil client-id user-id)))
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
			       (when (equalp *secret-message* message)
				 (progn
				   (debug-log-process "will take action~%")
				   (setf last-ret (funcall batch-process-function))))
			       (when (equalp *done-message* message)
				 (progn
				   (format logger "done all processing on port ~a~%" port)
				   (return))))))
			 (progn
			   (debug-log-process "timed out for port:~a using last-ret:~a" port last-ret)
			   (setf last-ret (funcall batch-process-function)))))))
	   (progn
	     (format logger  "closing socket for port ~a~%" port)
	     (usocket:socket-close socket)))))
     :name (format nil *batch-processor-id* port))))

(defun kill-batch-processor(thread)
  (format *standard-output* "will kill:~a~%" thread)
  (bordeaux-threads:destroy-thread thread))

(defun kill-all-batch-processors()

  (let ((threads (loop for thread in (bordeaux-threads:all-threads) when (~ "batch-processor" (bordeaux-threads:thread-name thread)) collecting thread)))

    (notify-batch-processor-done "127.0.0.1" *batch-script-port*)
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

  (start *hunchentoot-server*)
  (format t "Hunchentoot server started on port ~a~%" *hunchentoot-port*)

  (start-batch-processor "127.0.0.1" *batch-script-port* #'batch-script)
  (format t "started-batch processor ~%")

  (start-swank)

  ;; Shut down Hunchentoot
  (format t "Stopping Hunchentoot...port ~a~%" *hunchentoot-port*)
  (princ "Stopping Hunchentoot...")(terpri)
  (when *hunchentoot-server* (hunchentoot:stop *hunchentoot-server*))
  (stop-swank))

(defun test-grid()
  (destructuring-bind (distinct-users-last-week distinct-users-last-month distinct-users-lifetime downloads-last-week downloads-last-month batch-email-users batch-emails-sent) (aggregates) 
    (% "~a~%~a~%~a~%"
       (show-scalar-grid "Distinct Downloading Users" `(("Last Week" ,distinct-users-last-week) ("Last Month" ,distinct-users-last-month) ("Overall" ,distinct-users-lifetime)))
       (show-scalar-grid "Downloads" `(("Last Week" ,downloads-last-week) ("Last Month" ,downloads-last-month))) 
       (show-scalar-grid "Batch Emails" `(("Downloading Users" ,batch-email-users) ("Users Batch Emailed" ,batch-emails-sent))))))
	
(defun test-cl-who(my-stream)
  (with-html-output (my-stream)
    (loop for (link . title) in '(("http://zappa.com/" . "Frank Zappa")
				  ("http://marcusmiller.com/" . "Marcus Miller")
				  ("http://www.milesdavis.com/" . "Miles Davis"))
       do (htm (:a :href link
		   (:b (str title)))
	       :br))))
