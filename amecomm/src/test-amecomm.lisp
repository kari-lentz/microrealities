(require :ironclad)
(require :drakma)
(require :utility)
(require :flexi-streams)
(require :my-db)

(defpackage :test-amecomm
  (:use :common-lisp :utility)
  (:export run))

(in-package :test-amecomm)

(defparameter *testcomm* "comm2.amemusic.com:4430")
;(defparameter *testcomm* "comm.amemusic.com")
(defparameter *update-id* "MU787")
(defparameter *test-file* "Binary/Ame/A Marshmallow World.mp3.gz")
(defparameter *big-test-file* "Binary/Ame/BIG.zip")
(defparameter *update-folder* (format nil "/home/klentz/receiver/AME/Updates/~a" *update-id* ))
(defparameter *storage-file* (format nil "~a~a" *update-folder* *test-file*))
(defparameter *file-folder* (namestring (make-pathname :directory (pathname-directory (pathname *storage-file*)))))
(ensure-directories-exist *storage-file*)
(defparameter *max-buffer-size* 65536)
(defparameter *put-file-dir* "/home/klentz/amecomm/AME/Logs/AME_126623-test/")
;(defparameter *put-file-test* "BIG.zip")
(defparameter *put-file-test* "Communication Settings.dat.gz")
(defparameter *bigger-put-file-test* "music.mp3")
(defparameter *upload-file-url* nil) 
(defparameter *upload-file-path* nil)
(defparameter *upload-file-hash-string* nil)

(defparameter *raw-latin*
  (flexi-streams:make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")


(defun calc-hash( pf )
  (apply #'concatenate 'string (map 'list (lambda(x)(format nil "~2,'0x" x)) (ironclad:digest-file :md5 pf :buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8)) :start 0 :end *max-buffer-size*))))

(defun inspect-file( fp )

  (let ((num-bytes 64))
    (let ((data-buffer (make-array num-bytes :element-type '(unsigned-byte 8) :initial-element 0)))
      (let ((hash-str (apply #'concatenate 'string (map 'list (lambda(x)(format nil "~2,'0x" x))(ironclad:digest-file :md5 fp :buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8)) :start 0 :end *max-buffer-size*)))))
	(with-open-file (stream fp :element-type '(unsigned-byte 8))
	  (let ((file-length (file-length stream)))
	    (read-sequence data-buffer stream :end num-bytes)
	    (values file-length hash-str data-buffer)))))))
 
(defun comm-test-real()
  (let ((update-id "CM30775")(path "/Audio/Music/RU/A Marshmallow World.mp3.gz")(seek 5))(let ((my-array (make-array 32 :element-type '(unsigned-byte 8))))(multiple-value-bind (stream return-code headers) (drakma:http-request "https://comm.amemusic.com/Receiver/DownloadFile2.aspx" :parameters `(("update" . ,update-id) ("path" . ,path)) :additional-headers `(("seek" . ,seek)) :want-stream t) (read-sequence my-array stream) (list return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) (close stream) my-array)))))
	   
(defun basic-comm-test-vm()
  (drakma:http-request *testcomm*))

(defun download-file-part (target &key (seek 0) end hash file-length)
  (let ((my-buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8)))(headers ({})))
    (progn
      (setf ([] headers "seek") seek)
      (setf ([] headers "end") end)
      (setf ([] headers "Content-Hash") hash)
      (setf ([] headers "File-Length") file-length) 
      (let ((headers (remove-if-not (lambda(assoc) (cdr assoc)) (mapcar (lambda(key) (cons key ([] headers key))) (hash-table-keys headers)))))
	(multiple-value-bind (stream return-code headers) 
	    (drakma:http-request (format nil "https://~a/Receiver/DownloadFile2.aspx" *testcomm*) :parameters `(("update" . ,*update-id*) ("path" . ,*test-file*)) :additional-headers headers :want-stream t) 
	  (with-open-file (fs target :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
	    (let ((bytes-total 0))
	      (tagbody
	       begin-loop
		 (let ((bytes-read (read-sequence my-buffer stream)))
		   (incf bytes-total bytes-read)
		   (write-sequence my-buffer fs :end bytes-read)
		   (when (= bytes-read *max-buffer-size*) (go begin-loop))))
	      (close stream)
	      (values return-code headers bytes-total))))))))
  
(defun download-one-file-test(&optional (seek 0) (end nil))
  (multiple-value-bind (return-code headers total-bytes) (download-file-part *storage-file* :seek seek :end end) 
    (multiple-value-bind (file-length client-hash first-bytes) (inspect-file *storage-file*)
      (format nil "return code:~a~%headers:~a~%server-hash:~a~%~%file-length:~a~%:client-hash:~a~%first-bytes:~a~%total-bytes:~a~%" return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) file-length client-hash first-bytes total-bytes))))

(defun download-already-done-test()
  (with-output-to-string (res-stream)
    (multiple-value-bind (return-code headers total-bytes) (download-file-part *storage-file*) 
      (multiple-value-bind (file-length client-hash first-bytes) (inspect-file *storage-file*)
	(format res-stream "return code:~a~%headers:~a~%server-hash:~a~%~%file-length:~a~%:client-hash:~a~%first-bytes:~a~%total-bytes:~a~%" return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) file-length client-hash first-bytes total-bytes)
	(multiple-value-bind (return-code headers total-bytes) (download-file-part *storage-file* :hash client-hash :seek file-length)(format res-stream "return code: ~a~% headers: ~a~%total-bytes:~a~%" return-code headers total-bytes))))))

(defun download-obsolete-version-test()           
  (multiple-value-bind (return-code headers total-bytes) (download-file-part *storage-file* :hash "bogus-hash") 
    (multiple-value-bind (file-length client-hash first-bytes) (inspect-file *storage-file*)
      (format nil "return code:~a~%headers:~a~%server-hash:~a~%~%file-length:~a~%:client-hash:~a~%first-bytes:~a~%total bytes:~a~%" return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) file-length client-hash first-bytes total-bytes))))

(defun download-multi-parts( parts )
  (labels ((file-path(n)(format nil "~atemp-part-~2,'0d" *file-folder* n)))
    (let ((part-size (* 65536 4)))
      (dotimes (n parts)
	(download-file-part (file-path n) :seek (* n part-size) :end (* (1+ n) part-size)))
      (download-file-part (file-path parts) :seek (* parts part-size)))
    (let ((final-file (format nil "~afinal" *file-folder*)))
      (apply 'merge-files final-file (loop for n from 0 to parts collecting (file-path n)))
      (inspect-file final-file))))

(defun download-comm-test-vm(&optional (seek 0) (end nil))
  (let ((my-buffer (make-array *max-buffer-size* :element-type '(unsigned-byte 8))))
    (multiple-value-bind (stream return-code headers) 
	(drakma:http-request (format nil "https://~a/Receiver/DownloadFile2.aspx" *testcomm*) :parameters `(("update" . ,*update-id*) ("path" . ,*test-file*)) :additional-headers (cons (cons "seek" seek) (if end (cons (cons "end" end) nil) nil)) :want-stream t) 
      (with-open-file (fs *storage-file* :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
	(tagbody
	 begin-loop
	   (let ((bytes-read (read-sequence my-buffer stream)))
	     (write-sequence my-buffer fs :end bytes-read)
	     (when (= bytes-read *max-buffer-size*) (go begin-loop)))))
      (close stream)
      (multiple-value-bind (file-length client-hash first-bytes) (inspect-file *storage-file*)
	(format nil "return code:~a~%headers:~a~%server-hash:~a~%~%file-length:~a~%:client-hash:~a~%first-bytes:~a~%" return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) file-length client-hash first-bytes)))))

(defun download-big-file-test(&optional (seek 0) (end nil))
  (let ((*test-file* *big-test-file*))
    (let ((*storage-file* (format nil "~a~a" *update-folder* *test-file*)))	  
      (multiple-value-bind (return-code headers) (download-file-part *storage-file* :seek seek :end end) 
	(multiple-value-bind (file-length client-hash first-bytes) (inspect-file *storage-file*)
	  (format nil "return code:~a~%headers:~a~%server-hash:~a~%~%file-length:~a~%:client-hash:~a~%first-bytes:~a~%" return-code headers (cdr (assoc :Content-Hash headers :test #'equalp)) file-length client-hash first-bytes))))))

(defun put-test()
  (let ((pf (format nil "~a~a" *put-file-dir* *put-file-test*))(uri (ppcre:regex-replace " " (format nil "https://testcomm.amemusic.com/Receiver/Logs/AME_123456/~a.upl" *put-file-test*) "%20")))
    (format t "~a~%" uri)
    (with-open-file (in-stream pf :element-type '(unsigned-byte 8))
      (drakma:http-request uri :method :put :content in-stream :additional-headers `(("Content-Hash" . ,(calc-hash pf))) ))))

(defun persistent-put-test()
  (let ((http-stream (nth-value 4 (drakma:http-request "https://testcomm.amemusic.com/" :close nil))))
    (let ((pf (format nil "~a~a" *put-file-dir* *put-file-test*))(uri (ppcre:regex-replace " " (format nil "https://testcomm.amemusic.com/Receiver/Logs/AME_123456/~a.upl" *put-file-test*) "%20")))
      (format t "~a~%" uri)
      (with-open-file (in-stream pf :element-type '(unsigned-byte 8))
	(drakma:http-request uri :method :put :content in-stream :close nil :additional-headers `(("Content-Hash" . ,(format nil "~a" (calc-hash pf)))) :stream http-stream)
      (with-open-file (in-stream pf :element-type '(unsigned-byte 8))
	(drakma:http-request uri :method :put :content in-stream :additional-headers `(("Content-Hash" . ,(calc-hash pf))) :stream http-stream))))))

(defun bigger-put-test()
  (let ((pf (format nil "~a~a" *put-file-dir* *bigger-put-file-test*))(uri (ppcre:regex-replace " " (format nil "https://testcomm.amemusic.com/Receiver/Logs/AME_123456/~a.upl" *bigger-put-file-test*) "%20")))
    (format t "~a~%" uri)
    (with-open-file (in-stream pf :element-type '(unsigned-byte 8))
      (drakma:http-request uri :method :put :content in-stream :additional-headers `(("Content-Hash" . ,(calc-hash pf)))))))


(defun url-encode( url )
  (ppcre:regex-replace " " (format nil url *put-file-test*) "%20"))

(defun try-something(num-attempts function-to-try)
  (loop for attempt from 1 to num-attempts do (when (funcall function-to-try) (return))))

(defun launch-uploader(uri pf hash-string)
  (bordeaux-threads:make-thread 
   (lambda()
     (try-something 
      5
      (lambda()(if (eq (nth-value 1 (with-open-file (in-stream *upload-file-path* :element-type '(unsigned-byte 8))
				      (drakma:http-request *upload-file-url* :method :put :content in-stream :additional-headers `(("Content-Hash" . ,*upload-file-hash-string*))))) 200) (progn t) (progn (sleep 5) nil)))))
   :name (format nil "uploader:~a" pf) :initial-bindings `((*upload-file-url* . ,uri) (*upload-file-path* . ,pf)(*upload-file-hash-string* . ,hash-string))))

(defun threaded-put-test()
  (let ((pf (format nil "~a~a" *put-file-dir* *put-file-test*))(uri (url-encode (format nil "https://testcomm.amemusic.com/Receiver/Logs/AME_123456/~a.upl" *put-file-test*))))
    (format t "~a~%" uri)
    (let ((hash-str (calc-hash pf)))
      (launch-uploader uri pf hash-str)))) 

(defun run()
  (format t "do nothing stub~%"))

(defun test-lock(file-path)
  (with-open-file (fo file-path :direction :output :if-does-not-exist :create :if-exists nil)
    (when fo
      (progn
	(format fo "this is an earnest teast~%")
	(format t "made it in")
	fo))))

(defun test-open-file()
  (flet ((perform-open()
	   (with-open-file (in-stream "/home/klentz/not-here")
	     (let ((length (file-length in-stream)))
	       (let ((contents (make-string length)))
		 (read-sequence contents in-stream :start 0 :end length)
		 contents)))))
    (tagbody start-loop
       (restart-case
	   (perform-open)
	 (retry-open()
	   (go start-loop))
	 (barf()
	   (format t "barfing ~%"))))))

(defun test-error-handling()
  (let ((ctr 0))
    (handler-bind 
	((file-error (lambda(err)
		       (if (< ctr 5)
			 (progn
			   (format t "caught the error ~a and will sleep 'n retry ~%" err)
			   (incf ctr)
			   (sleep 1)
			   (invoke-restart 'retry-open))
			 (progn
			   (format t "caught ~a will file this away~%" err)
			   (invoke-restart 'barf))))))
    (test-open-file))))

(defun test-flexi()

  (let ((sample-buffer (map 'vector (lambda(x)(code-char x)) (range 256))))
    
    (let ((pf "/home/klentz/test.bin"))
      (with-open-file (out-stream pf :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)

	(progn
	  (setq out-stream (flexi-streams:make-flexi-stream out-stream :external-format *raw-latin*))
	  (write-line "this is line 1" out-stream)
	  (write-line "this is line 2" out-stream)
					;(setf (flexi-streams:flexi-stream-external-format fo) *raw-latin*)
	  (write-sequence sample-buffer out-stream)))

      (with-open-file (in-stream pf :direction :input :element-type '(unsigned-byte 8))
	(let ((length (file-length in-stream)))
	  (let ((buffer (make-array length)))
	    (read-sequence buffer in-stream)
	    (format t "read the stream~%" )
	    (file-position in-stream 0)
	    (read-sequence buffer in-stream)
	    (format t "read the stream again~%" )))))))

(defun test-connections()
  (let ((stream *standard-output*))
    (loop for n from 1 to 1000
       do
	 (let ((othread 
		(bordeaux-threads:make-thread 
		 (lambda()
		   (progn
		     (my-db:with-remote-db (:host "127.0.0.1" :user-id "root" :password "topdog" :database "AMECOMM") (format stream "~a~%" (my-db:query-remote "select * from REMOTE_AME_SYSTEM")))
		     (format stream "~a~%" (my-db:query-remote "select * from REMOTE_AME_SYSTEM")))) 
		 :name "mysql"))) 
	 
	   (bordeaux-threads:join-thread othread)))))

(defun test-persistent-connection()
  (let ((in-stream (drakma:http-request "https://comm.amemusic.com/" :close nil :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format in-stream) *raw-latin*)
    (let ((buffer (make-string *max-buffer-size*)))
      (with-output-to-string (out-stream nil :element-type '(unsigned-byte 8))
	(setq out-stream (flexi-streams:make-flexi-stream out-stream :external-format *raw-latin*))
	(loop
	   (let ((num-bytes (read-sequence buffer in-stream :end *max-buffer-size*)))
	     (format t "buffer:~a~%" buffer)
	     (write-sequence buffer out-stream :end num-bytes)
	     (unless (= num-bytes *max-buffer-size*) (return)))))
      (close in-stream))))

(defun test-persistent-connection()
  (let ((in-stream (nth-value 4 (drakma:http-request "https://comm.amemusic.com/" :close nil))))
    (unwind-protect
	 ;(drakma:http-request "https://comm.amemusic.com/")
	 (drakma:http-request "https://comm.amemusic.com/" :stream in-stream)
      (close in-stream))))

