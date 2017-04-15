;;;; nightmare.lisp

(in-package #:nightmare)

;;; "nightmare" goes here. Hacks and glory await!
(defparameter *width* 640)
(defparameter *height* 480)

(defclass nightmare (buffer)
  ((background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)))

(defmethod start-game ((nightmare nightmare))
  ;; set up level
  )

(defun nightmare ()
  ;;Configure the screen dimensions
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  (with-session
    (open-project :nightmare)
    (index-pending-resources)
    (let ((nightmare (make-instance 'nightmare)))
      (switch-to-buffer nightmare)
      (start-game nightmare))))
