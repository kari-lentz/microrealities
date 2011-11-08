(defpackage :my-env
  (:documentation "A for general purpose functions and macros")
  (:use cl)
  (:export :*remote-host* :*remote-user-id* :*remote-password* :*remote-database*  :*local-host* :*local-user-id* :*local-password* :*local-database*))

(in-package :my-env)

(defparameter *remote-host* "10.1.100.11")
(defparameter *remote-user-id* "thdirect")
(defparameter *remote-password* "topdog")
(defparameter *remote-database* "Web")

(defparameter *local-host* "192.168.0.185")
(defparameter *local-user-id* "sa")
(defparameter *local-password* "topdog")
(defparameter *local-database* "Web")