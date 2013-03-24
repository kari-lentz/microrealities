(in-package :star-catalog)

(defun str-handler(x)
  x)

(defun int-handler(x)
  (values (parse-integer x)))
	   
(defun float-handler(x)
  (values (read-from-string x)))

(defparameter *type-stream-decoders* ({} ('string #'str-handler) ('integer #'int-handler ) ('float #'float-handler) ('double-float #'float-handler)))

(defun parse-any( str type )
  (funcall ([] *type-stream-decoders* type) str))

(defeasyclass star-entry () (sid star-name ra dec magnitude color-index) :export-all nil)

(defclass star-catalog ()
  ((stars :initform nil :initarg :stars :accessor stars)))

(defparameter *star-catalog-system-path* (pathname "/home/klentz/runtime/astrolib/"))

(defparameter *star-csv* (merge-pathnames "stars.csv" *star-catalog-system-path*))
(defparameter *star-store* (merge-pathnames "stars.store" *star-catalog-system-path*))
    
(defun extract-objects(pf &key (regex-line-delim "[\\r\\n]+") (regex-fs ",") class-factory field-specs filter-p)
  (with-open-file (fo pf)
    (write-line "obtaining byte stream")
    (let ((file-bytes (make-array (file-length fo))))
      (read-sequence file-bytes fo)
      (write-line "obtaining rows of arrays") 
      (let ((lines (subseq (loop for line in (ppcre:split regex-line-delim (coerce file-bytes 'string)) collecting
	   (vmap (ppcre:split regex-fs line))) 1)))
	(write-line "running class factory")
	(let ((rows (loop for line in lines collecting 
			 (apply class-factory (loop for (type idx) in field-specs collecting 
						   (handler-case (parse-any (aref line idx) type) (error () nil)))))))
	  (write-line "running filter")
	  (if filter-p (remove-if-not filter-p rows) rows))))))
		    
(defun install-stars()
  (let ((stars  
	 (extract-objects *star-csv* 
			  :class-factory #'star-entry 
			  :field-specs '( (integer 0) (string 6) (double-float 7) (double-float 8) (double-float 10) (double-float 13))
			  :filter-p (lambda(o) (and (not (eq (sid o) 0)) (<= (magnitude o) 8.0))))))
    (cl-store:store stars *star-store*)))

(defun initialize-stars(&optional filter)
  (handler-bind
      ((file-error (lambda(err)(declare (ignore err))(invoke-restart 'install-it))))
    (let ((stars
	   (restart-case
	       (cl-store:restore *star-store*)
	     (install-it()
	       (install-stars)))))
      (if filter 
	  (remove-if-not filter stars)
	  stars))))