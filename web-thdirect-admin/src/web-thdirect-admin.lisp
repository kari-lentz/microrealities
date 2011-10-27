(defpackage :web-thdirect-admin
  (:use :common-lisp :hunchentoot :cl-who :utility :thdirect-admin)
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
	    (cond ((equal modifier "add")
		   (progn
		     (add-user client-id first-name last-name :email email :user-id (make-nil-str user-id) :user-password (make-nil-str user-password) :welcome-email-str welcome-email-str)))
		  ((equal modifier "delete")
		   (progn
		     (delete-user client-id user-id)))
		  (t
		     (update-user client-id user-id user-password first-name last-name email)))))))))

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
	  (:a :href "http://www.tophitsdirect.com/install/EmbedDemo.html" "http://www.tophitsdirect.com/install/EmbedDemo.html"))
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
      (fout "To install Top Hits Direct go to the following link:  http://www.tophitsdirect.com/install/EmbedDemo.html")
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

(define-easy-handler (account-welcome :uri "/account/welcome"
                                :default-request-type :post)
    ((user-id :parameter-type 'string)
     (user-password :parameter-type 'string)
     (email-address :parameter-type 'string))

  (if (and user-id user-password email-address)
      (if (send-email email-address user-id user-password)
	  "SUCCESS"
	  "INCOMPLETE-INFO")))
	
(defun get-welcome-stragglers()
  (with-databases
    (loop for (user-id user-password email-address) in (query-local "select user_id, user_password, email_address from WEB_USER where welcome_email is null and email_address like '%@%' and client_id <> 100") do 
	 (send-email email-address user-id user-password))))

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
