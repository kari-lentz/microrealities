;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start-hunchentoot.lisp
;;;;
;;;; Author:  William Bruschi
;;;; Date:    02-14-2009
;;;;
;;;; Starts Hunchentoot and Swank, then listens for a shutdown
;;;; command on the specified port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :amecomm)
(amecomm:run)

;(require :web-servers)
;(require :amecomm)
;(require :web-thdirect-admin)

;(web-servers:run)
;(amecomm:run)
;(web-thdirect-admin:run)
;(web-servers:wait)
;(web-thdirect-admin:stop-server)
;(amecomm:stop-server)
;(web-servers:stop-server)
