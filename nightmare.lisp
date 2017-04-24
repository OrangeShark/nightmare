;;;; nightmare.lisp

(in-package #:nightmare)

;;; "nightmare" goes here. Hacks and glory await!
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *level-width* 40)  ;; in units
(defparameter *level-height* 30) ;; in units
(defparameter *units* 32)
(defun units (n) (* *units* n))

(defparameter *tiles-file* "cave.png")

(defresource *tiles-files*
  :tile-height 16
  :tile-width 16)

(defun draw-image* (name sx sy swidth sheight dx dy dwidth dheight)
  (let* ((image (find-resource-object name))
         (image-width (sdl:width image))
         (image-height (sdl:height image)))
    (let ((tex-x1 (/ sx image-width))
          (tex-y1 (/ sy image-height))
          (tex-x2 (/ (+ sx swidth -1) image-width))
          (tex-y2 (/ (+ sy sheight -1) image-height))
          (dx2 (+ dx dwidth))
          (dy2 (+ dy dheight)))
      (xelf::enable-texture-blending)
      (set-blending-mode :alpha)
      (gl:bind-texture :texture-2d (find-texture name))
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


(defparameter *tile-units* 16)
(defun tile-units (n) (* n *tile-units*))
(defun create-tile-data (id)
  (multiple-value-bind (y x)
      (floor id *tile-units*)
    (list (tile-units x) (tile-units y))))

(defun random-tile ()
    (random-choose '(112 144 144 145 145 160 160 161 161)))

(defun random-ground-tiles (n acc)
  (if (<= n 0)
      acc
      (random-ground-tiles (- n 1) (cons (random-tile) acc))))

(defparameter *map-data-id* (random-ground-tiles 1200 '()))
(defparameter *map-data* (map 'vector #'create-tile-data *map-data-id*))




(defclass nightmare (buffer)
  ((player :initform (make-instance 'player))
   (background-color :initform '(25 23 22))
   (tile-sheet :initform "cave.png")
   (map-data :initform *map-data*)
   (tile-sheet-width :initform 16)
   (tile-sheet-height :initform 16)
   (width-unit :initform *level-width*)
   (height-unit :initform *level-height*)
   (width :initform (units *level-width*))
   (height :initform (units *level-height*))))

(defun draw-tiles (buffer tile-sheet)
  (with-slots (map-data width-unit) buffer
    (loop for i from 0
          while (< i (length map-data))
          do (let* ((tile (elt map-data i))
                    (tile-x (first tile))
                    (tile-y (second tile)))
               (multiple-value-bind (y x)
                   (floor i width-unit)
                 (draw-image* tile-sheet tile-x tile-y
                              *tile-units* *tile-units*
                              (units x) (units y)
                              *units* *units*))))))

(defmethod draw ((nightmare nightmare))
  (with-buffer nightmare
    (with-slots (objects width height background-color tile-sheet) nightmare
      (when background-color
        (draw-box 0 0 width height :color background-color))
      (when tile-sheet
        (draw-tiles nightmare tile-sheet))
      (if (or (shell-p nightmare)
              (z-sort-p nightmare))
          (xelf::draw-object-layer-z-sorted nightmare)
          (xelf::draw-object-layer nightmare)))))

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
