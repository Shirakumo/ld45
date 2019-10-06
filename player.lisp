(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 player-mesh) mesh
    (make-triangle 32 32 :orientation :right))

(define-global +move-speed+ 512)
(define-global +dragging-move-speed+ 64)
(define-global +takedown-max-distance+ 40)
(define-global +takedown-dot-threshold+ -0.9)
(define-global +dragging-max-distance+ 24)
(define-global +bullet-speed+ 2048)
(define-global +bullet-cooldown-time+ 1)

(define-shader-subject player (vertex-entity moving)
  ((name :initform :player)
   (vertex-array :initform (asset 'ld45 'player-mesh))
   (move-timer :initform 0 :accessor move-timer)))

(defmethod contained-p ((point vec2) (player player))
  (let ((loc (location player))
        (half-size (bsize player)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod step :before ((player player) ev)
  (let* ((vel (velocity player))
         (dt (dt ev))
         (move-speed (case (state player)
                       (:dragging +dragging-move-speed+)
                       (T +move-speed+))))
    (loop for device in (cl-gamepad:devices)
          for move = (vec (cl-gamepad:axis device 0)
                          (cl-gamepad:axis device 1))
          do (when (< 0.3 (vlength move))
               (setf (vx move) (* (vx move) (vx move) (signum (vx move)) (cl-gamepad:axis-multiplier device 0)))
               (setf (vy move) (* (vy move) (vy move) (signum (vy move)) (cl-gamepad:axis-multiplier device 1)))
               (nv+ vel (nv* move move-speed dt))))
    (cond
      ((< 0 (move-timer player))
       (decf (move-timer player) dt))
      (t
       (when (retained 'movement :left)
         (decf (vx vel) (* dt 0.7 move-speed)))
       (when (retained 'movement :right)
         (incf (vx vel) (* dt 0.7 move-speed)))
       (when (retained 'movement :up)
         (incf (vy vel) (* dt 0.7 move-speed)))
       (when (retained 'movement :down)
         (decf (vy vel) (* dt 0.7 move-speed)))))))

(defmethod die ((player player))
  (format T "~&
 _____  ______  _____  _____ ______ 
|_   _| |  _  \\|_   _||  ___||  _  \\
  | |   | | | |  | |  | |__  | | | |
  | |   | | | |  | |  |  __| | | | |
 _| |_  | |/ /  _| |_ | |___ | |/ / 
 \\___/  |___/   \\___/ \\____/ |___/  "))

(defmethod collide ((player player) (guard guard) hit)
  (when (eql :chase (state guard))
    (die player)))

(define-handler (player attempt-takedown) (ev)
  (for:for ((entity over +world+))
    (when (and (typep entity 'guard)
               (not (eq :chase (state entity)))
               (< (vdistance (location player) (location entity))
                  +takedown-max-distance+)
               (< (v. (angle-point (angle entity))
                      (nvunit (v- (location player) (location entity))))
                  +takedown-dot-threshold+))
      (v:info :player "Took down ~a" entity)
      (down entity)
      ;; You can only take down one at a time
      (for:end-for))))

(define-handler (player toggle-dragging) (ev)
  (case (state player)
    (:dragging
     (setf (state player) NIL))
    ((NIL)
     (for:for ((entity over +world+))
       (when (and (draggable-p entity)
                  (< (vdistance (location player) (location entity))
                     +dragging-max-distance+))
         (drag entity player)
         ;; You can only drag one guard
         (for:end-for))))))

(define-shader-subject bullet (vertex-entity moving)
  ((vertex-array :initform (asset 'ld45 'player-mesh))))

(defmethod step :before ((bullet bullet) ev)
  (setf (velocity bullet)
        (v* (angle-point (angle bullet))
            (* (dt ev) 0.7 +bullet-speed+))))

(defmethod collide ((bullet bullet) (guard guard) hit)
  (unless (eq (state guard) :down)
    (down guard)
    (leave bullet +world+)))

(defmethod collide ((bullet bullet) (wall wall) hit)
  (leave bullet +world+))

(define-handler (player shoot) (ev)
  (when (and (<= (move-timer player) 0)
             (zerop (vlength (velocity player))))
    (let ((bullet (make-instance 'bullet
                                 :location (vcopy (location player))
                                 :angle (angle player))))
      (enter bullet +world+)
      (setf (move-timer player) +bullet-cooldown-time+))))
