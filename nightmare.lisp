;;;; nightmare.lisp

(in-package #:nightmare)

;;; "nightmare" goes here. Hacks and glory await!
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *level-width* (units 40))
(defparameter *level-height* (units 30))
(defparameter *units* 32)
(defun units (n) (* *units* n))

(defparameter *tiles-file* "cave.png")

(defresource *tiles-files*
  :tile-height 16
  :tile-width 16)

(defun draw-image* (image sx sy swidth sheight dx dy dwidth dheight)
  (let ((image-width 256)
        (image-height 256))
    (let ((tex-x1 (/ sx image-width))
          (tex-y1 (/ sy image-height))
          (tex-x2 (/ (+ sx swidth) image-width))
          (tex-y2 (/ (+ sy sheight) image-height))
          (dx2 (+ dx dwidth))
          (dy2 (+ dy dheight)))
      (xelf::enable-texture-blending)
      (set-blending-mode :alpha)
      (gl:bind-texture :texture-2d (find-texture image))
      (xelf::set-vertex-color "white")
      (gl:with-primitive :quads
        (gl:tex-coord tex-x1 tex-y1)
        (gl:vertex dx dy)
        (gl:tex-coord tex-x2 tex-y1)
        (gl:vertex dx2 dy)
        (gl:tex-coord tex-x2 tex-y2)
        (gl:vertex dx2 dy2)
        (gl:tex-coord tex-x1 tex-y2)
        (gl:vertex dx dy2)))))

(defclass player (node)
  ((direction :initform nil)
   (height :initform (units 1))
   (width :initform (units 1))
   (color :initform "white")))

(defparameter *player-speed* 3)

(defmethod update ((player player))
  (with-slots (direction) player
    (setf direction (arrow-keys-direction))
    (when direction
      (move player (direction-heading direction) (* *player-speed* 2)))))

(defclass monster (node)
  ((direction :initform nil)
   (width :initform (units 1))
   (height :initform (units 1))
   (color :initform "green")))

(defmethod update ((monster monster))
  )

(defclass wall (node)
  ((color :initform "gray50")))

(defmethod collide ((player player) (wall wall))
  (with-slots (direction) player
    (setf direction (opposite-direction direction))
    (move player (direction-heading direction) (* *player-speed* 2))))

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
    (with-new-buffer
      (insert (make-wall left top (- right left) (units 1)))
      (insert (make-wall left bottom (- right left (units -1)) (units 1)))
      (insert (make-wall left top (units 1) (- bottom top)))
      (insert (make-wall right top (units 1) (- bottom top (units -1))))
      (current-buffer))))

(defclass nightmare (buffer)
  ((player :initform (make-instance 'player))
   (background-color :initform nil)
   (width :initform *level-width*)
   (height :initform *level-height*)))

(defmethod draw :before ((nightmare nightmare))
  (with-buffer nightmare
    (xelf::project-window nightmare)
    (with-slots (width height) nightmare
      (draw-box 0 0 width height :color "gray20")
      (draw-image* "cave.png" 0 144 16 16 (units 4) (units 4) (units 1) (units 1)))))

(defmethod start-game ((nightmare nightmare))
  ;; set up level
  (with-slots (player) nightmare
    (with-buffer nightmare
      (insert player)
      (move-to player 110 200)
      (follow-with-camera nightmare player)
     ; (paste-from nightmare (make-border 0 0 (- *level-width* (units 1)) (- *level-height* (units 1))))
      )))

(defparameter *title-string* "nightmare")

(defun nightmare ()
  ;;Configure the screen dimensions
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  (setf *window-title* *title-string*)
  (with-session
    (open-project :nightmare)
    (index-all-images)
    (index-pending-resources)
    (let ((nightmare (make-instance 'nightmare)))
      (switch-to-buffer nightmare)
      (start-game nightmare))))
