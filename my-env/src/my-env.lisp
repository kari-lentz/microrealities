(defpackage :my-env
  (:documentation "A for general purpose functions and macros")
  (:use cl)
  (:export :*remote-host* :*remote-user-id* :*remote-password* :*remote-database*  :*local-host* :*local-user-id* :*local-password* :*local-database* :*thdirect-admin-files* :*wav-path* :*working-path* :*archive-path* :*update-path* :*home-code* :*home-amecomm-content* :*amecomm-files* :*home-thdirect-content* :*home-thdirect-update-content* :*home-thdirect-music-files* :*thdirect-update-server*))

(in-package :my-env)

(defparameter *remote-host* "10.1.100.11")
(defparameter *remote-user-id* "thdirect")
(defparameter *remote-password* "topdog")
(defparameter *remote-database* "Web")

(defparameter *local-host* "192.168.0.185")
(defparameter *local-user-id* "sa")
(defparameter *local-password* "topdog")
(defparameter *local-database* "Web")

(defparameter *home-code* "/home/klentz/cl/")

(defparameter *thdirect-admin-files* "/home/klentz/web-thdirect-admin/")
(defparameter *thdirect-update-files* "/home/klentz/web-thdirect-admin/")

(defparameter *wav-path* "/mnt/ame-file-02/rpm-media-processing/Top Hits Production/" )

(defparameter *home-amecomm-content* "/home/klentz/amecomm/")
(defparameter *amecomm-files* "/home/klentz/amecomm/AME/")
(defparameter *home-thdirect-content* "/home/klentz/web-thdirect-admin/")
(defparameter *home-thdirect-update-content* "/home/klentz/web-thdirect-update/")

(defparameter *thdirect-update-server* "http://192.168.0.114:8081")
(defparameter *home-thdirect-music-files* "/home/klentz/MUSIC/")