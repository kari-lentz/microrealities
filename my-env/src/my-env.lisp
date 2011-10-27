(defpackage :my-env
  (:documentation "A for general purpose functions and macros")
  (:export :reset)
  (:use :cl :utility))

(in-package :my-env)

(defconstant +remote-host+ "10.1.100.11")
(defconstant +remote-user-id+ "thdirect")
(defconstant +remote-password+ "topdog")
(defconstant +remote-database+ "Web")

(defconstant +local-host+ "192.168.0.185")
(defconstant +local-user-id+ "sa")
(defconstant +local-password+ "topdog")
(defconstant +local-database+ "Web")

(defparameter utility:*remote-host* +remote-host+)
(defparameter utility:*remote-user-id* +remote-user-id+)
(defparameter utility:*remote-password* +remote-password+)
(defparameter utility:*remote-database* +remote-database+)

(defparameter utility:*local-host* +local-host+)
(defparameter utility:*local-user-id* +local-user-id+)
(defparameter utility:*local-password* +local-password+)
(defparameter utility:*local-database* +local-database+)

(defun reset()
  (setq utility:*remote-host* +remote-host+)
  (setq utility:*remote-user-id* +remote-user-id+)
  (setq utility:*remote-password* +remote-password+)
  (setq utility:*remote-database* +remote-database+)

  (setq utility:*local-host* +local-host+)
  (setq utility:*local-user-id* +local-user-id+)
  (setq utility:*local-password* +local-password+)
  (setq utility:*local-database* +local-database+))

