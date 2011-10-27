;(require :utility)
;(require :ironclad)

(defpackage :thdirect-admin
  (:documentation "A for general purpose functions and macros")
  (:use :cl :cl-who :utility)
  (:export :repeat-params 
	   :make-user-id
	   :make-password
	   :validate-user-id
	   :make-valid-user-id
	   :add-user
	   :update-user
	   :delete-user
	   :send-email
	   :email-error))

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
  (let ((rows (with-local-db-conn
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

(defun add-user-local( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-p) (created-by "SYSTEM"))

  (let ((user-id-final (or user-id (make-user-id first-name last-name))))
    (let ((user-password-final (or user-password (make-password client-id user-id-final))))

      (query-local (% "delete from Web.dbo.WEB_USER where client_id = ~a and user_id = ~a" (format-sql client-id) (format-sql user-id-final))) 
      (query-local (% "insert into Web.dbo.WEB_USER (USER_ID, USER_PASSWORD, CLIENT_ID, FIRST_NAME, LAST_NAME, COMPANY_NAME, TELEPHONE, EMAIL_ADDRESS, CREATED_DATE, CREATED_BY, WELCOME_EMAIL) values (~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, getdate(), ~a, ~a)" (format-sql user-id-final) (format-sql user-password-final) (format-sql client-id) (format-sql first-name) (format-sql last-name) (format-sql company-name) (format-sql telephone) (format-sql email) (format-sql created-by) (format-sql welcome-email-p))))))

(defun add-user-remote( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-p nil) (created-by "SYSTEM"))

  (let ((user-id-final (or user-id (make-user-id first-name last-name))))
    (let ((user-password-final (or user-password (make-password client-id user-id-final))))

      (query-remote (% "delete from WEB_USER where client_id = ~a and user_id = ~a" (format-sql client-id) (format-sql user-id-final))) 
      (query-remote (% "insert into WEB_USER (USER_ID, USER_PASSWORD, CLIENT_ID, FIRST_NAME, LAST_NAME, COMPANY_NAME, TELEPHONE, EMAIL_ADDRESS, CREATED_DATE, CREATED_BY, WELCOME_EMAIL) values (~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, now(), ~a, ~a )" (format-sql user-id-final) (format-sql user-password-final) (format-sql client-id) (format-sql first-name) (format-sql last-name) (format-sql company-name) (format-sql telephone) (format-sql email) (format-sql created-by) (format-sql welcome-email-p))))))

(defun update-user-local(client-id user-id user-password first-name last-name email &optional (company-name nil) (telephone nil))
  (query-local (repeat-params (% "update Web.dbo.WEB_USER set user_password = ~a, first_name = ~a, last_name= ~a, email_address = ~a, company_name = ~a, telephone = ~a where client_id = ~a and user_id = ~a") format-sql user-password first-name last-name email company-name telephone client-id user-id)))

(defun update-user-remote(client-id user-id user-password first-name last-name email &optional (company-name nil) (telephone nil))
  (query-remote (repeat-params (% "update WEB_USER set user_password = ~a, first_name = ~a, last_name= ~a, email_address = ~a, company_name = ~a, telephone = ~a where client_id = ~a and user_id = ~a") format-sql user-password first-name last-name email company-name telephone client-id user-id)))

(defun delete-user-local(client-id user-id)
  (query-local (repeat-params (% "delete from Web.dbo.WEB_USER where client_id = ~a and user_id = ~a") format-sql client-id user-id)))

(defun delete-user-remote(client-id user-id)
  (query-remote (repeat-params (% "delete from WEB_USER where client_id = ~a and user_id = ~a") format-sql client-id user-id)))

(defun add-user( client-id first-name last-name &key (email nil) (company-name nil) (telephone nil) (user-id nil) (user-password nil) (welcome-email-str nil) (created-by "SYSTEM"))

  (let ((user-id-final (or user-id (make-user-id first-name last-name))))
    (let ((user-password-final (or user-password (make-password client-id user-id-final)))) 
      (let ((welcome-email-p (if (find welcome-email-str (list "Y" "F") :test #'equal) welcome-email-str nil)))
	(add-user-local client-id first-name last-name :email email :company-name company-name :telephone telephone :user-id user-id-final :user-password user-password-final :welcome-email-p welcome-email-p :created-by created-by)
	(add-user-remote client-id first-name last-name :email email :company-name company-name :telephone telephone :user-id user-id-final :user-password user-password-final :welcome-email-p welcome-email-p :created-by created-by)))))

(defun update-user(client-id user-id user-password first-name last-name email &optional (company-name nil) (telephone nil))
  (update-user-local client-id user-id user-password first-name last-name email company-name telephone)
  (update-user-remote client-id user-id user-password first-name last-name email company-name telephone))

(defun delete-user(client-id user-id)
  (delete-user-local client-id user-id)
  (delete-user-remote client-id user-id))

(defun compose-html-email(user-id user-password)
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
      (:p "Tom Krikorian, President")))))

(defun compose-text-email(user-id user-password) 
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
      
      (fout "Tom Krikorian, President"))))

(define-condition email-error 
    (error)
  ((address :initarg :email-address
	:reader email-error-address)
   (object :initarg :error-object
	:reader email-error-object))
  (:report (lambda (condition stream)
             (format stream "email error->address:~a ~a" (email-error-address condition) (email-error-object condition)))))

(defun send-email(email-address user-id user-password)
  (with-databases
    (progn
      (handler-case
	  (cl-smtp:send-email
	   +mail-server+
	   "tophitsdirect@tophitsusa.com"
	   email-address
	   "Welcome to Top Hits Direct"
	   (compose-text-email user-id user-password)
	   :html-message (compose-html-email user-id user-password))
	(error(error-o)
	  (progn
	    (let ((sql (% "update WEB_USER set WELCOME_EMAIL = 'F' where user_id = '~a'" user-id)))
	      (query-remote sql)
	      (query-local sql))
	    (error (make-condition 'email-error :email-address email-address :error-object error-o))))) 
      (let ((sql (% "update WEB_USER set WELCOME_EMAIL = 'Y' where user_id = '~a'" user-id)))
	(query-remote sql)
	(query-local sql))
      T)))

(defun parse-name( full-name )
  (let ((mo (~ "(.+)[\\t\\s]+([^\\t\\s]+)$" full-name)))
    (if mo
	(values (funcall mo 1) (funcall mo 2))
	(values full-name nil))))

(defun add-subscription( client-id subscription-id user-id )

  (with-databases
    (query-local (repeat-params (% "delete from Billing.dbo.SUBSCRIPTION_THDIRECT where client_id = ~a and subscription_id = ~a and user_id = ~a") format-sql client-id  subscription-id user-id ))
    (query-local (repeat-params (% "insert into Billing.dbo.SUBSCRIPTION_THDIRECT (CLIENT_ID, SUBSCRIPTION_ID, USER_ID) values ( ~a, ~a, ~a )") format-sql client-id  subscription-id user-id ))))

(defun batch-create( &optional (quantity 1) )
  (with-databases 
    (let ((client-ids (query-local (% "select top ~a s.client_id, s.subscription_id from Billing.dbo.SUBSCRIPTION s where s.status='A' and s.client_cost > 0 and (s.suspend_from_date is null or s.suspend_from_date > getdate()) and (s.suspend_to_date is null or s.suspend_to_date < getdate()) and exists ( select * from Billing.dbo.CLIENT c, Billing.dbo.SHIP_ADDRESS sa  where c.client_id = s.client_id and c.client_id = sa.client_id and c.status='A' and sa.email like '%@%') and not exists ( select * from Web.dbo.WEB_USER wu where wu.client_id = s.client_id) order by s.client_id" quantity))))
      (loop for (client-id subscription-id full-name email) in 
	   (loop for (client-id subscription-id) in client-ids collecting 
		  (car (query-local (% "select top 1 c.client_id, ~a, sa.attention, sa.email from Billing.dbo.client c, Billing.dbo.ship_address sa where c.client_id = sa.client_id and sa.client_id = ~a and sa.email like '%@%'" subscription-id client-id))))
	 collecting 
	   
	   (multiple-value-bind (first-name last-name) (parse-name full-name)
	     (let ((user-id (make-valid-user-id first-name last-name)))
	       (let ((user-password (make-password client-id user-id)))
		 (progn
		   (add-user client-id first-name last-name :user-id user-id :user-password user-password :email email :created-by "BATCH-MAILING")
		   (add-subscription client-id subscription-id user-id)))))))))
