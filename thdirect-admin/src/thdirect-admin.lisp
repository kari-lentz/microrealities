;(require :utility)
;(require :ironclad)

(defpackage :thdirect-admin
  (:documentation "A for general purpose functions and macros")
  (:use :cl :cl-who :utility :my-db)
  (:export :repeat-params 
	   :make-user-id
	   :make-password
	   :validate-user-id
	   :make-valid-user-id
	   :add-user
	   :update-user
	   :delete-user
	   :send-email-welcome
	   :send-email-prospect
	   :email-error
	   :aggregates
	   :batch-create-welcome
	   :touch-all-expiration-dates
	   :batch-create-prospect
	   :promo-pack-replies))

(in-package :thdirect-admin)

(defparameter +mail-server+ "localhost")

(defmacro repeat-params(fmain frepeat &rest args)
  `(,@fmain ,@(loop for arg in args collecting `(,frepeat ,arg))))

(defun make-user-id( first-name last-name )
  (flet ((name-style( s )
	   (if s
	       (let ((len (length s)))
		 (cond ((>= len 2) (% "~a~a" (string-upcase(subseq s 0 1)) (string-downcase (subseq s 1))))
		       ((= len 1) (string-upcase (subseq s 0)))
		       (t "")))
	       ""))
	 (fix-bad-chars( s ) (ppcre:regex-replace-all "[^a-zA-Z0-9_]" s "")))
			  
    (fix-bad-chars (if (and last-name (> (length last-name) 0))
		       (% "TH_~a~a" (if (and first-name (> (length first-name) 0)) (string-upcase (subseq first-name 0 1)) "") (name-style last-name))
		       (% "TH_~a" (name-style first-name))))))

(defun make-password( client-id user-id )
  (let ((client-id-str (% "~a" client-id)))
    (let ((th-cipher (ironclad:make-cipher :RC2 :key (str-to-ubyte "THDIR") :mode :CTR :initialization-vector (str-to-ubyte"12345678")))
	  (secret (if (and (>= (length client-id-str) 3) (>= (length user-id) 3))
		      (% "~a~a" (subseq user-id 3) (subseq client-id-str 0 3) )
		      (% "~a~a" user-id client-id-str))))
      (let ((res-place (make-array (length secret) :element-type '(unsigned-byte 8))))
	(ironclad:encrypt th-cipher (str-to-ubyte secret) res-place)
	(let ((str (ubyte-to-str (map '(vector (unsigned-byte 8)) (lambda(x) (let ((res (mod x 36)))
									       (cond ((< res 10) (+ 48 res))
										     (t (+ res 87))))) res-place))))
	  (if (> (length str) 8) (subseq str 0 8) str))))))
      
(defun validate-user-id( user-id )
  (let ((rows (with-local-db () 
		(query-local (% "select * from WEB_USER where user_id = ~a" (format-sql user-id))))))
    (if rows nil t)))
	  
(defun make-valid-user-id( first-name last-name &optional (user-ids nil))
  (let ((orig-user-id (make-user-id first-name last-name)))
    (let ((gen-user-id orig-user-id))
      (do ((idx 2 (1+ idx))) 
	  ((and 
	    (not (find gen-user-id user-ids :test #'equal))
	    (validate-user-id gen-user-id)))
	(setf gen-user-id (% "~a~a" orig-user-id idx)))
      gen-user-id)))

(defun query-expiration-date( client-id user-id )

  (read-mssql-date (with-databases 
		     (car (car (query-local (% "select EXPIRATION_DATE from BILLING.dbo.QRY_THDIRECT_EXPIRATION_DATES where CLIENT_ID = ~a and USER_ID = ~a" (format-sql client-id) (format-sql user-id))))))))

(defun add-user-local( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-p nil) (expiration-date nil) (created-by "SYSTEM"))
  
  (let ((user-id (or user-id (make-user-id first-name last-name)))(welcome-email welcome-email-p)(email-address email)(created-date (make-dts-now)))
    (let ((user-password (or user-password (make-password client-id user-id))))
      
      (query-local (make-db-delete web.dbo.web-user (client-id user-id)))
      (query-local (make-db-insert web.dbo.web-user (client-id user-id user-password first-name last-name company-name telephone email-address created-by created-date welcome-email expiration-date))))))

(defun add-user-remote( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-p nil) (expiration-date nil) (created-by "SYSTEM"))
  
  (let ((user-id (or user-id (make-user-id first-name last-name)))(welcome-email welcome-email-p)(email-address email)(created-date (make-dts-now)))
    (let ((user-password (or user-password (make-password client-id user-id))))
      
      (query-remote (make-db-delete web-user (client-id user-id)))
      (query-remote (make-db-insert web-user (client-id user-id user-password first-name last-name company-name telephone email-address created-by created-date welcome-email expiration-date))))))

(defun update-user-local(set-fields key-fields)
  (query-local (make-db-update "Web.dbo.WEB_USER" set-fields key-fields)))

(defun update-user-remote(set-fields key-fields)
  (query-remote (make-db-update "WEB_USER" set-fields key-fields)))

(defun delete-user-local(client-id user-id)
  (query-local (make-db-delete web.dbo.web-user (client-id user-id))))

(defun delete-user-remote(client-id user-id)
  (query-remote (make-db-delete web-user (client-id user-id))))

(defun add-user( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-str nil)(created-by "SYSTEM"))

  (let ((user-id (or user-id (make-user-id first-name last-name))))
    (let ((user-password (or user-password (make-password client-id user-id)))
	  (expiration-date)) 
      (let ((welcome-email-p (if (find welcome-email-str (list "Y" "F") :test #'equal) welcome-email-str nil)))
	(add-user-local client-id first-name last-name :email email :company-name company-name :telephone telephone :user-id user-id :user-password user-password :welcome-email-p welcome-email-p :expiration-date expiration-date :created-by created-by)
	(add-user-remote client-id first-name last-name :email email :company-name company-name :telephone telephone :user-id user-id :user-password user-password :welcome-email-p welcome-email-p :expiration-date expiration-date :created-by created-by)))))

(defun update-user(set-fields client-id user-id)
  (with-databases
    (let ((set-fields (cons 
		       (make-db-field "EXPIRATION_DATE" (query-expiration-date client-id user-id)) 
		       set-fields))
	  (where-fields (make-db-fields client-id user-id)))

      (update-user-local set-fields where-fields)
      (update-user-remote set-fields where-fields))))

(defun delete-user(client-id user-id)
  (delete-user-local client-id user-id)
  (delete-user-remote client-id user-id))

(defun compose-html-email-welcome(user-id user-password)
  (with-html-output-to-string (*standard-output* nil)
    (:html 
     (:head (:style :type "text/css" "strong.th{font-weight:bold;}strong.th-acc{font-weight:bold;color:#ff0000}strong.th-extra{color:green;font-weight:bold;text-decoration:underline}"))
     (:body
      (:p (:strong :class "th" "Dear Valued Top Hits U.S.A. Client - "))
      (:br)

      (:p "First, let me thank you for your continued good business.  We appreciate you and we look forward to continuing to deliver the best products and customer service.")
      (:br)

      (:p "We are very pleased to inform you of another enhancement to your Top Hits music update service.  Now you can access the new hit releases directly by using our new application Top Hits Direct.  You can now download the newest Top Hits as they are added, often on a daily basis.")  
      (:br)

      (:p (:strong "Best of all, you will still receive your present disc service with no changes.  There is " (:strong :class "th-extra" "no added cost")" for the addition of Top Hits Direct! "))
      (:br)

      (:p (:div "To install Top Hits Direct, click on the following link:"))
      (:p
	  (:a :href "http://www.tophitsdirect.com/install/welcome.psp" "http://www.tophitsdirect.com/install/welcome.psp"))
      (:br)
	  
      (:p "User ID:" (:strong :class "th-acc" (str user-id)))
      (:p  "Password:" (:strong :class "th-acc" (str user-password)))
      (:br)
      (:p "You can change your password as needed.")
      (:br)

      (:p (:strong "Your monthly service rate will not change&nbsp;") "with the addition of Top Hits Direct.&nbsp;&nbsp;"  (:strong :class "th-extra" "You will continue to receive your discs.") "&nbsp;&nbsp;If you subscribe to our Music Video service, you will continue to receive the videos on disc just as before.  The videos will be offered on the Top Hits Direct service sometime in December or January.")
      (:br)

      (:p "If you have any questions, please email tophitsdirect@tophitsusa.com or call 800-521-2537.  We look forward to serving you and thank you again for your business!")
      (:br)
      (:p "Best regards,")
      (:br)
      (:p "Top Hits USA Team")))))

(defun compose-text-email-welcome(user-id user-password) 
  (with-output-to-string (fo)
    (flet ((fout(&optional (s ""))
	     (write-line s fo)))
      (fout "Dear Valued Top Hits U.S.A. Client - ")
      (fout)
      (fout "First, let me thank you for your continued good business.  We appreciate you and we look forward to continuing to deliver the best products and customer service.")
      (fout)
      (fout "We are very pleased to inform you of another enhancement to your Top Hits music update service.  Now you can access the new hit releases directly by using our new application Top Hits Direct.  You can now download the newest Top Hits as they are added, often on a daily basis.")  
      (fout)
      (fout "Best of all, you will still receive your present disc service with no changes.  There is no added cost for the addition of Top Hits Direct! ")

      (fout)
      (fout "To install Top Hits Direct go to the following link:  http://www.tophitsdirect.com/install/welcome.psp")
      (fout)
      
      (fout (% "User ID:  ~a" user-id))
      (fout  (% "Password:  ~a" user-password))
      (fout)

      (fout "You can change your password as needed.")
      (fout)

      (fout "Your monthly service rate will not change with the addition of Top Hits Direct.  You will continue to receive your discs.  If you subscribe to our Music Video service, you will continue to receive the videos on disc just as before.  The videos will be offered on the Top Hits Direct service sometime in December or January.")

      (fout)

      (fout "If you have any questions, please email tophitsdirect@tophitsusa.com or call 800-521-2537.  We look forward to serving you and thank you again for your business!")

      (fout)

      (fout "Best regards,")

      (fout)
      
      (fout "Top Hits USA Team"))))

(defun compose-html-email-prospect(user-id user-password)
  (with-html-output-to-string (*standard-output* nil)
    (:html 
     (:head (:style :type "text/css" "strong.th{font-weight:bold;}strong.th-acc{font-weight:bold;color:#ff0000}strong.th-extra{color:green;font-weight:bold;text-decoration:underline}"))
     (:body
      (:p (:strong :class "th" "Dear Prospective Client - "))
      (:br)

      (:p "Thank you for your interest in the Top Hits USA subscription music service.  The information included in this email will familiarize you with the various service options we have available.")
      (:br)

      (:p "Please click on the following link to download/view all the information about Top Hits USA and services we have to offer.")  
      (:p
       (:a :href "http://www.tophitsusa.com/promo-pack" "http://www.tophitsusa.com/promo-pack"))
      (:br)

      (:p (:div "To install Top Hits Direct and start your free 10 day trial, click on the following link:"))
      (:p
	  (:a :href "http://www.tophitsdirect.com/install/welcome.psp" "http://www.tophitsdirect.com/install/welcome.psp"))
      (:br)
	  
      (:p "User ID:" (:strong :class "th-acc" (str user-id)))
      (:p  "Password:" (:strong :class "th-acc" (str user-password)))
      (:br)
      (:p "You can change your password as needed.")
      (:br)

      (:p "If you have any questions, please email tophitsdirect@tophitsusa.com or call 800-521-2537.  We look forward to serving you and thank you again for your business!")
      (:br)
      (:p "Best regards,")
      (:br)
      (:p "Top Hits USA Team")))))

(defun compose-text-email-prospect(user-id user-password) 
  (with-output-to-string (fo)
    (flet ((fout(&optional (s ""))
	     (write-line s fo)))
      (fout "Dear Prospective Client - ")
      (fout)
      (fout "Thank you for your interest in the Top Hits USA subscription music service.  The information included in this email will familiarize you with the various service options we have available.")
      (fout)
      (fout "Please click on the following link to download/view all the information about Top Hits USA and services we have to offer.")
      (fout)
      (fout "http://www.tophitsusa.com/promo-pack")
      (fout)

      (fout "We are very pleased to inform you of another enhancement to your Top Hits music update service.  Now you can access the new hit releases directly by using our new application Top Hits Direct.  You can now download the newest Top Hits as they are added, often on a daily basis.")  
      (fout)
      (fout "Best of all, you will still receive your present disc service with no changes.  There is no added cost for the addition of Top Hits Direct! ")

      (fout)
      (fout "To install Top Hits Direct and start your free 10 day trial go to the following link:  http://www.tophitsdirect.com/install/welcome.psp")
      (fout)

      (fout "To install Top Hits Direct, click on the following link: http://www.tophitsdirect.com/install/welcome.psp")
      (fout)
	  
      (fout (format nil "User ID:~a" user-id))
      (fout  (format nil "Password:~a" user-password))
      (fout)
      (fout "You can change your password as needed.")
      (fout)

      (fout "If you have any questions, please email tophitsdirect@tophitsusa.com or call 800-521-2537.  We look forward to serving you and thank you again for your business!")
      (fout)
      (fout "Best regards,")
      (fout)
      (fout "Top Hits USA Team"))))

(define-condition email-error 
    (error)
  ((address :initarg :email-address
	:reader email-error-address)
   (object :initarg :error-object
	:reader email-error-object))
  (:report (lambda (condition stream)
             (format stream "email error->address:~a ~a" (email-error-address condition) (email-error-object condition)))))

(defun send-email-welcome(email-address user-id user-password)
  (with-databases
    (progn
      (handler-case
	  (cl-smtp:send-email
	   +mail-server+
	   "tophitsdirect@tophitsusa.com"
	   email-address
	   "TOP HITS USA ... Welcome to Top Hits Direct"
	   (compose-text-email-welcome user-id user-password)
	   :html-message (compose-html-email-welcome user-id user-password))
	(error(error-o)
	  (progn
	    (let ((sql (% "update WEB_USER set WELCOME_EMAIL = 'F' where user_id = '~a'" user-id)))
	      (query-remote sql)
	      (query-local sql))
	    (error (make-condition 'email-error :email-address email-address :error-object error-o))))) 
      (let ((sql (% "update WEB_USER set WELCOME_EMAIL = 'Y', LAST_EMAILED = ~a where user_id = ~a" (format-sql (make-dts-now)) (format-sql user-id))))
	(query-remote sql)
	(query-local sql))
      T)))

(defun send-email-prospect(email-address user-id user-password)
  (with-databases
    (progn
      (handler-case
	  (cl-smtp:send-email
	   +mail-server+
	   "tophitsdirect@tophitsusa.com"
	   email-address
	   "TOP HITS USA ... Promo Pack"
	   (compose-text-email-prospect user-id user-password)
	   :html-message (compose-html-email-prospect user-id user-password))
	(error(error-o)
	  (progn
	    (let ((sql (% "update WEB_USER set WELCOME_EMAIL = 'F' where user_id = '~a'" user-id)))
	      (query-remote sql)
	      (query-local sql))
	    (error (make-condition 'email-error :email-address email-address :error-object error-o)))))
      (let ((now (format-sql (make-dts-now)))) 
	(let ((sql (% "update WEB_USER set PROMO_PDF_EMAIL_DTS = ~a, LAST_EMAILED = ~a where user_id = ~a" now now (format-sql user-id))))
	  (query-remote sql)
	  (query-local sql))
	T))))

(defun parse-name( full-name )
  (let ((mo (~ "(.+)[\\t\\s]+([^\\t\\s]+)$" full-name)))
    (if mo
	(values (funcall mo 1) (funcall mo 2))
	(values full-name nil))))

(defun add-subscription( client-id subscription-id user-id )

  (with-databases
    (query-local (repeat-params (% "delete from Billing.dbo.SUBSCRIPTION_THDIRECT where client_id = ~a and subscription_id = ~a and user_id = ~a") format-sql client-id  subscription-id user-id ))
    (query-local (repeat-params (% "insert into Billing.dbo.SUBSCRIPTION_THDIRECT (CLIENT_ID, SUBSCRIPTION_ID, USER_ID) values ( ~a, ~a, ~a )") format-sql client-id  subscription-id user-id ))))

(defun batch-create-welcome( &optional (quantity 1) )
  (with-databases 
    (let ((client-ids (query-local (% "select top ~a s.client_id, s.subscription_id from Billing.dbo.SUBSCRIPTION s where s.status='A' and s.client_cost >= 54.95 and (s.suspend_from_date is null or s.suspend_from_date > getdate()) and (s.suspend_to_date is null or s.suspend_to_date < getdate()) and exists ( select * from Billing.dbo.CLIENT c, Billing.dbo.SHIP_ADDRESS sa  where c.client_id = s.client_id and c.client_id = sa.client_id and c.status='A' and sa.email like '%@%') and not exists ( select * from Web.dbo.WEB_USER wu where wu.client_id = s.client_id) order by s.client_id" quantity))))
      (loop for (client-id subscription-id full-name email) in 
	   (loop for (client-id subscription-id) in client-ids collecting 
		  (car (query-local (% "select top 1 c.client_id, ~a, sa.attention, sa.email from Billing.dbo.client c, Billing.dbo.ship_address sa where c.client_id = sa.client_id and sa.client_id = ~a and sa.email like '%@%'" subscription-id client-id))))
	 collecting 
	   
	   (multiple-value-bind (first-name last-name) (parse-name full-name)
	     (let ((user-id (make-valid-user-id first-name last-name)))
	       (let ((user-password (make-password client-id user-id)))
		 (progn
		   (add-user client-id first-name last-name :user-id user-id :user-password user-password :email email :created-by "BATCH-MAILING")
		   (add-subscription client-id subscription-id user-id)
		   (update-user nil client-id user-id)))))))))

(defun aggregates()
  (with-remote-db () 
    (mapcar (lambda(r)(car ( car r)))
	    (list
	     (query-remote "select count(*) as num_users from WEB_USER wu where exists( select * from WEB_USER_HISTORY wh where wu.user_id = wh.user_id and DTS_DOWNLOADED >= DATE_ADD( NOW(), INTERVAL -1 WEEK))")
	     (query-remote "select count(*) as num_users from WEB_USER wu where exists( select * from WEB_USER_HISTORY wh where wu.user_id = wh.user_id and DTS_DOWNLOADED >= DATE_ADD( NOW(), INTERVAL -1 MONTH))")
	     (query-remote "select count(*) as num_users from WEB_USER wu where exists( select * from WEB_USER_HISTORY wh where wu.user_id = wh.user_id)")
	     (query-remote "select count(*) as num_downloads from WEB_USER_HISTORY where DTS_DOWNLOADED >= DATE_ADD( NOW(), INTERVAL -1 WEEK)")
	     (query-remote "select count(*) as num_downloads from WEB_USER_HISTORY where DTS_DOWNLOADED >= DATE_ADD( NOW(), INTERVAL -1 MONTH)")
	     (query-remote "select count(*) as num_users from WEB_USER wu where exists( select * from WEB_USER_HISTORY wh where wu.user_id = wh.user_id) and created_by = 'BATCH-MAILING'")
	     (query-remote "select count(*) as num_users from WEB_USER wu where created_by = 'BATCH-MAILING'")))))

(defun promo-pack-replies(dts-begin dts-end)
  (with-local-db () 
    (values
     (car (car (query-local "select count(*) as num_users from Web.dbo.WEB_USER wu where created_by = 'BATCH-PROMO' and promo_pdf_email_dts >= ~a and promo_pdf_email_dts < ~a" dts-begin dts-end)))
     (car (car (query-local "select count(*) as num_users from Web.dbo.WEB_USER wu where created_by = 'BATCH-PROMO' and promo_pdf_email_dts >= ~a and promo_pdf_email_dts < ~a and first_login_dts is not null" dts-begin dts-end)))
     (car (car (query-local "select count(*) as num_users from Web.dbo.WEB_USER wu where created_by = 'BATCH-PROMO' and promo_pdf_email_dts >= ~a and promo_pdf_email_dts < ~a and first_login_dts is not null and (expiration_date is null or expiration_date >= getdate())" dts-begin dts-end)))
     (car (car (query-local "select count(*) as num_users from Web.dbo.WEB_USER wu where created_by = 'BATCH-PROMO' and promo_pdf_email_dts >= ~a and promo_pdf_email_dts < ~a and first_login_dts is not null and exists (select * from CLIENT c inner join SUBSCRIPTION s on c.client_id = s.client_id where c.client_id = wu.client_id and c.status = 'A' and s.status = 'A')" dts-begin dts-end))))))


(defun get-non-users(&optional dts)
  (subseq (with-databases
    (query-remote (% "select wu.user_id, wu.user_password, wu.email_address from WEB_USER wu where (wu.last_emailed is null or wu.last_emailed > ~a) and wu.welcome_email = 'Y' and created_by in ('BATCH-MAILING','SYSTEM') and wu.email_address like '%@%' and wu.client_id <> 100 and not exists (select * from WEB_USER_HISTORY wh where wh.user_id = wu.user_id) and expiration_date >= NOW()" (format-sql (or dts "1900-01-01"))))) 0)) 

;(defun resend-promo()
;  (loop for (user-id user-password email-address) in (get-non-users (make-dts-now)) do
;       (handler-case
;	   (progn
;	     (send-email-welcome email-address user-id user-password)
;	     (format t "emailed ~a~%" email-address))
;	 (email-error(error-o)(log-error "error while ending email" error-o))
;	 (error (error-o) (format t "error while ending email: ~a~%" error-o)))))

(defun touch-all-expiration-dates()

  (with-databases
    (loop for (client-id user-id expiration-date) in (query-local (% "select CLIENT_ID, USER_ID, EXPIRATION_DATE from BILLING.dbo.QRY_THDIRECT_EXPIRATION_DATES order by CLIENT_ID, USER_ID" )) do 
	 (progn 
	   (query-remote (% "update WEB_USER set EXPIRATION_DATE = ~a where CLIENT_ID = ~a and USER_ID = ~a" (format-sql (read-mssql-date expiration-date)) (format-sql client-id) (format-sql user-id)))
	   (query-local (% "update WEB_USER set EXPIRATION_DATE = ~a where CLIENT_ID = ~a and USER_ID = ~a" (format-sql (read-mssql-date expiration-date)) (format-sql client-id) (format-sql user-id)))))))

(defun add-promo-account( client-id user-id trial-days)

  (with-local-db ()
    (query-local (format nil "update Billing.dbo.CLIENT set DOWNLOAD_USER_ID = ~a, TRIAL_DAYS = ~a where client_id = ~a" (format-sql user-id) (format-sql trial-days) (format-sql client-id)))))

(defun batch-create-prospect( &optional (quantity 1) (trial-days 10))
  (with-databases 
    (let ((client-ids (query-local (format nil "select top ~a c.client_id from Billing.dbo.CLIENT c where c.status='PR' and exists ( select * from Billing.dbo.SHIP_ADDRESS sa where c.client_id = sa.client_id and attention is not null and sa.email like '%@%') and not exists ( select * from Web.dbo.WEB_USER wu where wu.client_id = c.client_id) order by c.client_id" quantity))))
      (loop for (client-id full-name email) in 
	   (loop for (client-id) in client-ids collecting 
		(car (query-local (format nil "select top 1 c.client_id, sa.attention, sa.email from Billing.dbo.client c, Billing.dbo.ship_address sa where c.client_id = sa.client_id and sa.client_id = ~a and sa.email like '%@%'" client-id))))
	 collecting 
	   (multiple-value-bind (first-name last-name) (parse-name full-name)
	     (when (or first-name last-name)
	       (let ((user-id (make-valid-user-id first-name last-name)))
		 (let ((user-password (make-password client-id user-id)))
		   (progn
		     (add-user client-id first-name (or last-name "") :user-id user-id :user-password user-password :email email :created-by "BATCH-PROMO" :welcome-email-str "Y")
		     (add-promo-account client-id user-id trial-days)
		     (update-user nil client-id user-id))))))))))


(defun doctor-data()
  (with-databases ()
    (loop for (client-id user-id) in (query-remote "select client_id, user_id from WEB_USER where created_by <> 'BATCH-PROMO' order by client_id, user_id")
       do
	 (destructuring-bind (first-login-dts last-login-dts) (car (query-remote "select min(dts_downloaded), max(dts_downloaded) from WEB_USER_HISTORY where user_id = ~a" user-id))
	   (when first-login-dts
	     (let ((first-login-dts (read-mysql-date first-login-dts))(last-login-dts (read-mysql-date last-login-dts)))
	       (let ((sql (format nil "update WEB_USER set first_login_dts = ~a, last_login_dts = ~a where user_id = ~a" (format-sql first-login-dts) (format-sql last-login-dts) (format-sql user-id))))
		 (query-remote sql)
		 (query-local sql))))))))


(defun mass-email-prospects(quantity)

  (with-local-db ()
    (loop for (client-id user-id user-password email-address) in
	 (query-remote (format nil "select client_id, user_id, user_password, email_address from WEB_USER where created_by = 'BATCH-PROMO' and welcome_email = 'Y' and promo_pdf_email_dts is null order by client_id LIMIT ~a" quantity))
	 do 
	 (handler-case
	     (progn
	       (send-email-prospect email-address user-id user-password)
	       (format t "client-id: ~a user-id: ~a user-password: ~a email-address: ~a~%" client-id user-id user-password email-address ))
	   (email-error (error-object) (format t "caught ~a so will skip~%" error-object))))))