;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start-hunchentoot.lisp
;;;;
;;;; Author:  William Bruschi
;;;; Date:    02-14-2009
;;;;
;;;; Starts Hunchentoot and Swank, then listens for a shutdown
;;;; command on the specified port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :web-servers
  (:use :common-lisp :hunchentoot)
  (:export :run))

(in-package :web-servers)

(defparameter *hunchentoot-port* 8080)
(defparameter *shutdown-port* 6440)
(defparameter *swank-loader* "/home/klentz/cl/slime/swank-loader.lisp")
(defparameter *swank-port* 4006)

;;; Start the hunchentoot server
(defparameter *hunchentoot-server* (make-instance 'acceptor :port *hunchentoot-port*))

(defparameter hunchentoot:*access-log-pathname* "/home/klentz/log/access.log")
(defparameter hunchentoot:*message-log-pathname* "/home/klentz/log/error.log")

(defun run()

  (start *hunchentoot-server*)
  (princ "Hunchentoot server started on port ")
  (princ *hunchentoot-port*) (terpri)

;;; Start swank
  (swank-loader:init)
  (swank:create-server :port *swank-port* :dont-close t)
  (princ "Loaded Swank on port ")
  (princ *swank-port*)(terpri)

;;; Wait and listen for shutdown command
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			     :type :stream :protocol :tcp)))

    ;; Listen on a local port for a TCP connection
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) *shutdown-port*)
    (sb-bsd-sockets:socket-listen socket 1)

    ;; When it comes, close the sockets and continue
    (multiple-value-bind (client-socket addr port)
	(sb-bsd-sockets:socket-accept socket)
      (sb-bsd-sockets:socket-close client-socket)
      (sb-bsd-sockets:socket-close socket)
      (and addr addr)
      (and port port)))

  ;; Shut down Hunchentoot
  (princ "Stopping Hunchentoot...")(terpri)
  (stop *hunchentoot-server*)

  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:quit))