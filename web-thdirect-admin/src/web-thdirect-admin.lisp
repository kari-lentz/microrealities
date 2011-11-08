(defpackage :web-thdirect-admin
  (:use :common-lisp :hunchentoot :cl-who :utility :my-db :thdirect-admin)
  (:export stop-server test-cl-who))

(in-package :web-thdirect-admin)

(setf *dispatch-table*
      (list #'dispatch-easy-handlers
	    (create-folder-dispatcher-and-handler "/account/" "/home/klentz/admin/")
            #'default-dispatcher))

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
      (log-message 0 err-msg)
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

(define-easy-handler (account-welcome :uri "/account/welcome"
                                :default-request-type :post)
    ((user-id :parameter-type 'string)
     (user-password :parameter-type 'string)
     (email-address :parameter-type 'string))

  (handler-case
      (if (and user-id user-password email-address)
	  (progn
	    (send-email email-address user-id user-password)
	    "SUCCESS")
	  "INCOMPLETE-INFO")
    (email-error(error-o)(log-error "error while ending email" error-o))
    (error(error-o) (log-error "error while ending email" error-o))))
	
(defun get-welcome-stragglers()
  (with-databases
    (loop for (user-id user-password email-address) in (query-local "select user_id, user_password, email_address from WEB_USER where welcome_email is null and email_address like '%@%' and client_id <> 100") do
	 (handler-case
	     (send-email email-address user-id user-password)
	   (email-error(error-o)(log-error "error while ending email" error-o))
	   (error (error-o) (log-error "error while ending email" error-o))))))
	   

(define-easy-handler (account-welcome-batch :uri "/account/welcome-batch"
                                :default-request-type :get)
    ((verify-auth :parameter-type 'string))
  (when (equal verify-auth "THDIRECTISAGREATPRODUCT")
    (get-welcome-stragglers)))

(define-easy-handler (testing-email-html :uri "/testing/email/html"
				  :default-request-type :post)
    ()
  (compose-html-email "TH_KLentz" "roger22"))

(define-easy-handler (testing-email-text :uri "/testing/email/text"
				  :default-request-type :post)
    ()
  (compose-text-email "TH_KLentz" "roger22"))

	
(defun test-cl-who(my-stream)
  (with-html-output (my-stream)
    (loop for (link . title) in '(("http://zappa.com/" . "Frank Zappa")
				  ("http://marcusmiller.com/" . "Marcus Miller")
				  ("http://www.milesdavis.com/" . "Miles Davis"))
       do (htm (:a :href link
		   (:b (str title)))
	       :br))))
