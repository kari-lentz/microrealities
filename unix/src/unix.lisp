(defpackage :unix
  (:documentation "provides wrapper for trivial shell and other posic wrappers")
  (:use :cl :utility)
  (:export :trivial-shell-error :move-file :remove-file :list-files :glob :stat :touch :status-code +file-missing-status+))

(in-package :unix)

(defconstant +file-missing-status+ 1 "sell returns if file not there")

(define-condition trivial-shell-error(error)
  ((shell-cmd :initarg :shell-cmd :reader shell-cmd)
   (status-code :initarg :status-code :reader status-code)
   (description :initarg :description :reader description))  
  (:report (lambda (condition stream) (format stream "trivial shell error~% running cmd: ~a~%status-code:~a~%descr: ~a~%" (shell-cmd condition) (status-code condition) (description condition)))))

(defmacro define-trivial-shell-function(function-name (&rest lambda-list) shell-format-string)
  
  `(defun ,function-name(,@lambda-list &optional out-stream)
     (let ((cmd (format nil ,shell-format-string ,@lambda-list)))
       (multiple-value-bind (out err status)(trivial-shell:shell-command cmd)
	 (unless (= status 0)
	   (error (make-condition 'trivial-shell-error :shell-cmd cmd :status-code status :description err)))
	 (if out-stream 
	     (format out-stream "~a" out)
	     (with-open-file (null-out "/dev/null" :direction :output :if-exists :append)
	       (format null-out "~a" out)))))))

(define-trivial-shell-function move-file (src-file-path dest-file-path) "mv \"~a\" \"~a\"")
(define-trivial-shell-function remove-file (file-path) "rm \"~a\"")
(define-trivial-shell-function touch-low-level (year month day hour minute second file-path) "touch -t ~a~2,'0d~2,'0d~2,'0d~2,'0d.~2,'0d \"~a\"")
(define-trivial-shell-function list-files (dir-pattern) "ls -al \"~a\"")
(define-trivial-shell-function glob (dir-pattern) "ls -al ~a")

(defun touch( dts file-path )
  (multiple-value-bind (second minute hour day month year) (dts-parts dts) (touch-low-level year month day hour minute second file-path)))

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

(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))

(defun stat (filename)
  (values t
	  (make-dts-from-ut (+ (stat-last-access filename) *unix-epoch*))
	  (make-dts-from-ut (+ (stat-last-change filename) *unix-epoch*))
	  (make-dts-from-ut (+ (stat-last-modify filename) *unix-epoch*))))

;;; Directory Changes

