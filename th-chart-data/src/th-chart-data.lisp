(defpackage :th-chart-data
  (:documentation "A for general purpose functions and macros")
  (:use :cl :typeset :my-env :my-db :utility)
  (:export :run :chart-data-t :chart-data-c))

(in-package :th-chart-data)

(defparameter *artwork-path* (merge-pathnames "artwork/" *home-thdirect-content*))
(ensure-directories-exist *artwork-path*)

(defparameter *rpm-logo-path* "/home/klentz/artwork/rpm-logo.jpg")
(defparameter *mediabase-logo-path* "/home/klentz/artwork/mediabase.png")
(defparameter *my-background-color* '(0.8 1.0 0.8))
(defparameter *my-font* "Helvetica")
(defparameter *my-bold-font* "Helvetica-Bold")

(defconstant +raw-latin+
  (flexi-streams:make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defun report-date()
  (let ((now (make-dts-now)))
    (format nil "~a, ~a ~2,'0d, ~a" (dts-long-dow now) (dts-long-month now) (dts-day now) (dts-year now))))

(defmacro with-pdf-report(title file &body frms)

  (let ((sym-content (gensym)))
    `(pdf:with-document ()
       (pdf:with-page ()
	(setf (pdf::bounds pdf::*page*) (vector 0 0 792 595) )  
	(pdf:with-outline-level (,title (pdf:register-page-reference))
	   (let ((,sym-content 
		  (tt:compile-text ()
		    ,@frms)))
	     (tt::draw-block ,sym-content 20 575 700 545))
	   (let ((mediabase-logo (pdf:make-image *mediabase-logo-path*))) 
	     (pdf:add-images-to-page mediabase-logo)
	     (pdf:draw-image mediabase-logo 602 25 160 80 0 t)

	     (let ((helvetica (pdf:get-font "Helvetica-Bold")))
	       (pdf:in-text-mode
		 (pdf:set-font helvetica 10.0)
		 (pdf:move-text (+ 602 20) (+ 25 0) )
		 (pdf:draw-text (report-date)))))
	   (let ((,sym-content 
		  (tt:compile-text ()
		    (table (:col-widths '(160) :padding 0 :border 0)
		      (row ()
			(cell ()
			  (paragraph (:font *my-font* :font-size 5) (format-string "Copyright ~a Mediabase" (dts-year (make-dts-now))))))
		      (row ()
			(cell ()
			  (paragraph (:font *my-font* :font-size 5) "Reprinted with permission")))))))
	     (tt::draw-block ,sym-content 712 (+ 80 25) 160 80))))
       (pdf:write-document ,file))))

(defmacro with-gensyms((&rest ids) &body frms)
  `(let ( ,@(loop for id in ids collecting `(,id (gensym))))
     ,@frms))

(defmacro draw-chart-data(disc format)

  (let ((font-size 6))
    (with-gensyms (sym-rank sym-title sym-artist sym-label sym-disc-track)
      `(paragraph () "its little stuff like that ...")
      `(table ( :padding 0 :col-widths '(10 105 85 18 35) :background-color *my-background-color* :border 0 :cell-padding 0)
	 (header-row (:background-color *my-background-color*)
	   (cell (:col-span 4) (paragraph (:h-align :left :font *my-bold-font* :font-size ,font-size)
				 (format-string "RADIO AIRPLAY CHARTS FOR ~a FORMAT" ,format)))
	   (cell (:col-span 1) (paragraph (:h-align :left :font *my-bold-font* :font-size ,font-size)
				 (format-string "~aDISC#" ,disc))))
	 
	 (loop for (,sym-rank ,sym-title ,sym-artist ,sym-label ,sym-disc-track) in (query-remote "select cd.rank, m.title, case when m.first is null then m.artist else concat( m.first, ' ', m.artist) end as artist, left(m.label, 3) as label, m.disc from CHART_DATA cd, MUSIC m where cd.music_id = m.music_id and left( m.disc, 1) = ~a and cd.CHART = ~a order by rank" ,disc ,format)
	    do
	      (row (:background-color *my-background-color*)
		(cell () 
		  (paragraph (:h-align :left :font *my-font* :font-size ,font-size)
		    ,sym-rank))
		(cell () 		   
		  (paragraph (:h-align :left :font *my-font* :font-size ,font-size)
		    ,sym-title))
		(cell ()
		  (paragraph (:h-align :left :font *my-font* :font-size ,font-size)
		    ,sym-artist))
		(cell ()
		  (paragraph (:h-align :left :font *my-font* :font-size ,font-size)
		    ,sym-label))
		(cell () 
		  (paragraph (:h-align :left :font *my-font* :font-size ,font-size)
		    ,sym-disc-track))))))))

(defun chart-data(disc format-1 format-2 format-3 out-file)

  (with-remote-db ()
    (let ((tt::*default-page-orientation* :landscape)(report-width 759))
      (with-pdf-report "Chart Data" out-file       
	(table ( :padding 0 :col-widths (loop for i from 1 to 3 collecting (/ report-width 3)) :background-color :white :border 0 :cell-padding 0)
	  (header-row (:background-color :white)
	    (cell (:v-align :bottom) (image :file *rpm-logo-path* :dx 170 :dy 45 :inline t :offset 0))
	    (cell (:v-align :bottom) (paragraph (:font *my-bold-font* :font-size 18 :h-align :center) "Top Hits U.S.A."))
	    (cell (:v-align :bottom) (paragraph (:font *my-bold-font* :font-size 18 :h-align :right) "800-521-2537")))
	    
	  (row (:background-color *my-background-color*) 
	    (cell (:col-span 3) (hrule :dy 2)))
	  (row (:background-color *my-background-color*)
	    (cell () (draw-chart-data disc format-1))
	    (cell () (draw-chart-data disc format-2))
	    (cell () (draw-chart-data disc format-3))))))))

(defgeneric chart-data-t( stream ))

(defmethod chart-data-t( out-stream )
  (chart-data "T" "CTRY" "CHR" "AC" out-stream))

(defmethod chart-data-t( (out-stream stream) )
  (setq out-stream (flexi-streams:make-flexi-stream out-stream :external-format +raw-latin+))
  (call-next-method out-stream)) 

(defgeneric chart-data-c( stream ))

(defmethod chart-data-c( out-stream )
  (chart-data "C" "URB" "RK" "ALT" out-stream))

(defmethod chart-data-c( (out-stream stream) )
  (setq out-stream (flexi-streams:make-flexi-stream out-stream :external-format +raw-latin+))
  (call-next-method out-stream)) 

