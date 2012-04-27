
(defpackage :junk-mail
  (:documentation "A for general purpose functions and macros")
  (:use :cl :cl-who :utility :my-db :cl-smtp :cl-mime)
  (:export :run))

(in-package :junk-mail)

(defparameter *mail-server* "localhost")
(defparameter *mail-from* "tophitsdirect@tophitsusa.com")
(defparameter *VH-DROPPED* "VH-DROPPED")
(defparameter *WEB-UPGRADE* "WEB-UPGRADE")

(defun test-send-text(email-address)
  (with-smtp-mail (mail-stream *mail-server* *mail-from* (list email-address)) 
    (write-line "Subject:  My Test" mail-stream)
    (format mail-stream "to: ~a mail-stream~%" email-address)
    (write-line "Hello there this is a test message." mail-stream)
    (write-line "Hope you enjoyed this." mail-stream)))

(defun test-send-pdf(email-address)
  (with-open-file (in-stream "/home/klentz/test/vh-change-letter.pdf" :element-type '(unsigned-byte 8))
    (let ((file-length (file-length in-stream)))
      (let ((pdf-data (make-array file-length)))
	(read-sequence pdf-data in-stream) 
	(with-smtp-mail (mail-stream *mail-server* *mail-from* (list email-address))
	  (write-line "Subject:  My Test PDF" mail-stream)
	  (format mail-stream "to: ~a mail-stream~%" email-address)
	  (cl-mime:print-mime mail-stream (make-instance 'cl-mime:mime :type "application" :subtype "pdf" :content pdf-data) t t)))))) 

(defmacro with-text-email(&body frms)
  `(with-output-to-string (fo)
    (flet ((fout(&optional (s ""))
	     (write-line s fo)))
      (fout "Greetings Valued Top Hits U.S.A. Client!")
      (fout)
      ,@frms
      (fout)
      (fout "Best regards,")
      (fout)
      (fout "Top Hits USA Team"))))

(defmacro with-html-email(&body frms)
  `(with-html-output-to-string (*standard-output* nil)
    (:html 
     (:head (:style :type "text/css" "strong.th{font-weight:bold;}strong.th-acc{font-weight:bold;color:#ff0000}strong.th-extra{color:green;font-weight:bold;text-decoration:underline}"))
     (:body
      (:p (:strong :class "th" "Greetings Valued Top Hits U.S.A. Client!"))
      (:br)
      ,@frms
      (:br)
      (:p "Best regards,")
      (:br)
      (:p "Top Hits USA Team")))))

(defun send-junk-email(id subject compose-closure-html compose-closure-text client-id email-address)
  (with-local-db (:host "192.168.0.24" :database "Billing")
    (progn
      (handler-case
	  (progn
	    (cl-smtp:send-email
	     *mail-server*
	     "tophitsdirect@tophitsusa.com"
	     email-address
	     subject
	     (funcall compose-closure-text)
	     :html-message (funcall compose-closure-html))
	    (query-local "insert into JUNK_EMAIL (EMAIL_ID, CLIENT_ID, EMAIL, SUCCESS, DTS_SENT) values (~a, ~a, ~a, 'Y', getdate())" id client-id email-address)
	    (format t "successfully sent to ~a~%" email-address))
	(error(error-o)
	    (query-local "insert into JUNK_EMAIL (EMAIL_ID, CLIENT_ID, EMAIL, SUCCESS, DTS_SENT) values (~a, ~a, ~a, 'N', getdate())" id client-id email-address)
	  (format t "for email address:~a got error:~a~%" email-address error-o))))))

(defun compose-email-text-video()
  (with-text-email 
    (fout "We would like you to know about a change that is about to be made in your Top
Hits Video service.")
    (fout "We introduced the VHMpeg4 DVD ROM some time ago. It has been very well
received and most clients who used the original VH DVD elected to convert to
the Mpeg4 version. Therefore, effective with the June issue, we will no longer
produce the original DVD and convert entirely to the Mpeg4 DVD ROM. This
change will not affect your monthly cost and will offer these advantages over the
original DVD:")
    (fout)
    (fout "*Compatible with most popular DJ software including Virtual DJ and
Serato.")
    (fout "*Easily copy and paste videos to your hard drive.")
    (fout "*You will receive five extra bonus videos each month which include popular videos from the 70's to the early 2000's.")
    (fout)
    (fout "We greatly appreciate your good business and we hope this change provides you with even more excellent videos and greater convenience and quality.  If you have any questions, comments or concerns regarding this change, please do not hesitate to let us know by calling 800-521-2537, email tophitsdirect@tophitsdirect.com or Fax 888-776-0006.")))
 
(defun compose-email-html-video()
  (with-html-email
    (:p "We would like you to know about a change that is about to be made in your Top
Hits Video service.")
    (:p "We introduced the VHMpeg4 DVD ROM some time ago. It has been very well
received and most clients who used the original VH DVD elected to convert to
the Mpeg4 version. Therefore, effective with the June issue, we will no longer
produce the original DVD and convert entirely to the Mpeg4 DVD ROM. This
change will not affect your monthly cost and will offer these advantages over the
original DVD:")
    (:p "&#149Compatible with most popular DJ software including Virtual DJ and
Serato.")
    (:p "&#149Easily copy and paste videos to your hard drive.")
    (:p "&#149You will receive five extra bonus videos each month which include popular videos from the 70's to the early 2000's.")
    (:p "We greatly appreciate your good business and we hope this change provides you with even more excellent videos and greater convenience and quality.  If you have any questions, comments or concerns regarding this change, please do not hesitate to let us know by calling 800-521-2537, email tophitsdirect@tophitsdirect.com or Fax 888-776-0006.")))

(defun send-video-email(client-id email-address)
  (send-junk-email *vh-dropped* "TOP HITS USA VH DVD Disc" #'compose-email-html-video #'compose-email-text-video client-id email-address))

(defun mass-email-video(&optional (quantity 1))
  (with-local-db (:host "192.168.0.24" :database "Billing")
    (loop for (client-id email) in
	 (query-local "select top ~a client_id, email from ship_address sa where exists (select * from subscription s, subscription_item si, client c where s.subscription_id = si.subscription_id and si.client_id = c.client_id and sa.client_id = c.client_id and si.product_type='VH' and s.status = 'A' and c.status = 'A') and email is not null and email like '%%@%%' and not exists (select * from junk_email jm where sa.client_id = jm.client_id and sa.email = jm.email and email_id = ~a) order by client_id" quantity *vh-dropped*)
       do
	 (progn
	   (send-video-email client-id email)
	   (format t "~a ~a~%" client-id email)))))

(defun compose-email-text-th-web-upgrade()
  (with-text-email 
    (fout "We would like to take this opportunity to say THANK YOU for your
good business. Since 1970, RPM has been the premier provider of
the newest hit music in the U.S.A.")
    (fout "We are about to begin an upgrade of our website, TopHitsUSA.com.")
    (fout "As we evaluate the site and plan upgrades, we would appreciate
hearing from you. Your feedback will help us serve you even better.
Please feel free to tell us what you like and, maybe more importantly,
what you would like to see added or improved. If you have any
input, comments or questions, please share them with us with an
email to tophitsdirect@tophitsusa.com.")))

(defun compose-email-html-th-web-upgrade()
  (with-html-email
    (:p "We would like to take this opportunity to say THANK YOU for your
good business. Since 1970, RPM has been the premier provider of
the newest hit music in the U.S.A.")
    (:p "We are about to begin an upgrade of our website, TopHitsUSA.com.")
    (:p "As we evaluate the site and plan upgrades, we would appreciate
hearing from you. Your feedback will help us serve you even better.
Please feel free to tell us what you like and, maybe more importantly,
what you would like to see added or improved. If you have any
input, comments or questions, please share them with us with an
email to " (:a :href "mailto:tophitsdirect@tophitsusa.com?subject=RE: TOP HITS USA WEB UPGRADE" "tophitsdirect@tophitsusa.com")".")))

(defun send-email-th-web-upgrade(client-id email-address)
  (send-junk-email *WEB-UPGRADE* "TOP HITS USA WEB SITE UPGRADE" #'compose-email-html-th-web-upgrade #'compose-email-text-th-web-upgrade client-id email-address))

(defun mass-email-th-web-upgrade(&optional (quantity 1))
  (let ((num-sent 0))
    (with-local-db (:host "192.168.0.24" :database "Billing")
      (loop for (client-id email) in
	   (query-local "select top ~a sa.client_id, sa.email from client c, ship_address sa where c.ship_id = sa.ship_id and c.status = 'A' and exists (select * from subscription s where s.client_id = c.client_id and s.status = 'A') and email is not null and email like '%%@%%' and not exists (select * from junk_email jm where sa.client_id = jm.client_id and sa.email = jm.email and email_id = ~a) order by c.client_id" quantity *WEB-UPGRADE*)
	 do
	   (progn
	     (send-email-th-web-upgrade client-id email)
	     (format t "~a ~a~%" client-id email)
	     (incf num-sent))))
    (format t "In all, sent ~a emails~%" num-sent)))

(defun run()
  nil)
