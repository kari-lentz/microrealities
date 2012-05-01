(defpackage :my-env
  (:documentation "environment variables")
  (:use :cl)
  (:export :*remote-host*
	   :*remote-user-id*
	   :*remote-password*
	   :*remote-database*
	   :*local-host*
	   :*local-user-id*
	   :*local-password*
	   :*local-database*))

(in-package :my-env)

(defparameter *remote-host* "localhost")
(defparameter *remote-user-id* "root")
(defparameter *remote-password* "topdog")
(defparameter *remote-database* "WORLD")
(defparameter *local-host* "localhost")
(defparameter *local-user-id* "sa")
(defparameter *local-password* "topdog")
(defparameter *local-database* "WORLD")
(defparameter *local-stores* "/home/klentz/cl/stores/")
