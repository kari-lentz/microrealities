(require :ironclad)
(require :drakma)
(require :utility)
(require :flexi-streams)
(require :my-db)

(defpackage :test-recv
  (:use :common-lisp :utility)
  (:export run))

(in-package :test-recv)

(defparameter *comm-server* "https://comm2.amemusic.com:4430")
(defparameter *comm-server-prefix* (format nil "~a/Receiver" *comm-server*))

(defparameter *system-id* 61229)
(defparameter *tag* "AME_336610")

(defparameter *put-file-dir* "/home/klentz/amecomm/AME/Logs/AME_126623-test/")
(defparameter *put-file-test* "Communication Settings.dat.gz")

(defparameter *max-buffer-size* 65536)

(defparameter *my-thread-n* nil)
(defparameter *my-standard-output* *standard-output*)

(defun init-comm()
  (multiple-value-bind (content status-code) (drakma:http-request *comm-server*)
    (list content status-code)))

(defun get-timezone()
  (multiple-value-bind (content status-code) (drakma:http-request (format nil "~a/gettimezone.asp?system_id=~a" *comm-server-prefix* *system-id*))
    (list content status-code))) 

(defun get-server-time()
  (multiple-value-bind (content status-code) (drakma:http-request (format nil "~a/getservertime.asp" *comm-server-prefix*))
    (list content status-code)))

(defun get-expiration-date()
  (multiple-value-bind (content status-code) (drakma:http-request (format nil "~a/getsystemexpirationdate.asp?system_id=~a" *comm-server-prefix* *system-id*))
    (list content status-code)))

(defun set-current-versions()
  (multiple-value-bind (content status-code) (drakma:http-request (format nil "~a/SetCurrentVersions.aspx?system=~a&tag=~a&catalog=1992&profile=418" *comm-server-prefix* *system-id* *tag*))
    (list content status-code)))

(defun set-update-status()
  (multiple-value-bind (content status-code) (drakma:http-request (format nil "~a/SetUpdateStatus.aspx?system=~a&tag=~a&update=MU568&media=INTERNET&status=DONE&date=03/14/2012%2006:26:22%20AM"  *comm-server-prefix* *system-id* *tag*))
    (list content status-code)))

(defun calc-hash( pf )
  (apply #'concatenate 'string (map 'list (lambda(x)(format nil "~2,'0x" x)) (ironclad:digest-file :md5 pf :buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8)) :start 0 :end *max-buffer-size*))))

(defun upload-test()
  (multiple-value-bind (content status-code)
      (let ((pf (format nil "~a~a" *put-file-dir* *put-file-test*))(uri (ppcre:regex-replace " " (format nil "~a/Logs/~a/~a.upl" *comm-server-prefix* *tag* *put-file-test*) "%20")))
	(with-open-file (in-stream pf :element-type '(unsigned-byte 8))
	  (drakma:http-request uri :method :put :content in-stream :additional-headers `(("Content-Hash" . ,(calc-hash pf))))))
    (list content status-code)))

(defun comm-test-one()
  (handler-case
      (let ((ret 
	     (loop for func in
		  (list
		   #'init-comm
		   #'get-timezone
		   #'get-server-time
		   #'get-expiration-date
		   #'set-current-versions
		   #'set-update-status
		   #'upload-test)
		collecting (nth 1 (funcall func)))))
	(format *my-standard-output* "thread #~a complete: ~%" *my-thread-n*)
	ret)
    (error (error-object) (format *my-standard-output* "in thread #~acaught error ~a~%" *my-thread-n* error-object))))

(defun comm-test-many(num-comms)
  (loop for num from 1 to num-comms do
       (bordeaux-threads:make-thread #'comm-test-one :name (format nil "test-comm-recv #~a" num) :initial-bindings `((*my-thread-n* . ,num) (*my-standard-output* . ,*standard-output*))))) 

(defparameter *num-thread* nil)
(defparameter *my-stream* nil)

(defun test-mysql()
  (let ((threads 
	 (loop for n from 1 to 1000
	    collecting
	      (bordeaux-threads:make-thread 
	       (lambda()
		 (progn
		   (format *my-stream* "thread #~a returns ~a~%" *num-thread* (my-db:with-remote-db (:host "127.0.0.1" :user-id "root" :password "topdog" :database "AmeClient")  (length (my-db:query-remote "select * from AME_MUSIC where display_artist > 'J' order by music_id LIMIT 100")))))) 
	       :name "mysql" :initial-bindings `((*num-thread* . ,n)(*my-stream* . ,*standard-output*))))))
	 (loop for thread in threads do (bordeaux-threads:join-thread thread))))

(defun test-mssql()
  (let ((threads 
	 (loop for n from 1 to 1000
	    collecting
	      (bordeaux-threads:make-thread 
	       (lambda()
		 (progn
		   (format *my-stream* "thread #~a returns ~a~%" *num-thread* (my-db:with-local-db (:database "AmeMaster") (length (my-db:query-local "select * from RPM_CLIENT_SYSTEM_DISC where system_id = 60113"))))))
	       :name "mysql" :initial-bindings `((*num-thread* . ,n)(*my-stream* . ,*standard-output*))))))
    (loop for thread in threads do (bordeaux-threads:join-thread thread))))
