(in-package :cities)

(defun init-store(row-constructor store-file &optional filter)
  (handler-bind
      ((file-error (lambda(err)(declare (ignore err))(invoke-restart 'install-it))))
    (let ((ret
	   (restart-case
	       (cl-store:restore store-file)
	     (install-it()
	       (cl-store:store (funcall row-constructor) store-file)))))
      (if filter 
	  (remove-if-not filter ret)
	  ret))))

(defun parse-coord(mo-coord lat-string)
  (with-regex-matches mo-coord lat-string (degrees minutes seconds direction)
    (with-rebindings (degrees minutes seconds) #'read-from-string
      (* (+ degrees (/ minutes 60) (/ seconds 3600)) (if (~ "[NWnw]" direction) 1 -1)))))

(defun parse-population(population)
  (nth-value 0 (read-from-string (ppcre:regex-replace-all "\\s" population ""))))

(defparameter *my-opengl-system-path* (pathname "/home/klentz/runtime/my-opengl/"))
(defparameter *cities-txt* (merge-pathnames "cities.txt" *my-opengl-system-path*))
(defparameter *cities-store* (merge-pathnames "cities.store" *my-opengl-system-path*))

(defeasyclass city () (name population latitude longitude) :export-all nil)
		
(defun city-factory()
  
  (with-regexes ((mo-line "([0-9]+)\\\t(.*)\\\s\\:\\s([0-9\\s]+)\\\t([^\\\t]+)\\\t([^\\\t]+)([^\\\t]+)\\\t([^\\\t]+)\\\t([^\\\t]+)")(mo-coord "([0-9]+)°([0-9]+)'([0-9\\.]+)\\\"\\s([A-Z])"))
    (with-open-file (fo *cities-txt*)
      (loop for line = (read-line fo nil nil) while line collecting
	   (with-regex-matches mo-line line ((name 2) (population 3)(latitude 7)(longitude 8))
	     (city name (parse-population population) (parse-coord mo-coord latitude) (parse-coord mo-coord longitude)))))))

(defun initialize-cities(&optional filter)
  (init-store #'city-factory *cities-store* filter))
		
(defun run())

