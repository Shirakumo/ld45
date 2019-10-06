(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 player-mesh) mesh
    (make-triangle 32 32 :orientation :right))

(define-global +move-speed+ 512)
(define-global +dragging-move-speed+ 64)
(define-global +takedown-min-distance+ 16)
(define-global +takedown-max-distance+ 40)
(define-global +takedown-dot-threshold+ -0.9)
(define-global +dragging-max-distance+ 24)

(define-shader-subject player (vertex-entity moving)
  ((name :initform :player)
   (vertex-array :initform (asset 'ld45 'player-mesh))
   (dragged-entity :initform nil :accessor dragged-entity)))

(defmethod contained-p ((point vec2) (player player))
  (let ((loc (location player))
        (half-size (bsize player)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod step :before ((player player) ev)
  (let* ((loc (location player))
         (vel (velocity player))
         (dt (dt ev))
         (dragged-entity (dragged-entity player))
         (move-speed (if dragged-entity +dragging-move-speed+ +move-speed+)))
    (loop for device in (cl-gamepad:devices)
          for move = (vec (cl-gamepad:axis device 0)
                          (cl-gamepad:axis device 1))
          do (when (< 0.3 (vlength move))
               (setf (vx move) (* (vx move) (vx move) (signum (vx move)) (cl-gamepad:axis-multiplier device 0)))
               (setf (vy move) (* (vy move) (vy move) (signum (vy move)) (cl-gamepad:axis-multiplier device 1)))
               (nv+ vel (nv* move move-speed dt))))
    (when (retained 'movement :left)
      (decf (vx vel) (* dt 0.7 move-speed)))
    (when (retained 'movement :right)
      (incf (vx vel) (* dt 0.7 move-speed)))
    (when (retained 'movement :up)
      (incf (vy vel) (* dt 0.7 move-speed)))
    (when (retained 'movement :down)
      (decf (vy vel) (* dt 0.7 move-speed)))
    (when dragged-entity
      (vsetf (velocity dragged-entity) (vx vel) (vy vel)))))

(defmethod die ((player player))
  (format T "~&
 _____  ______  _____  _____ ______ 
|_   _| |  _  \\|_   _||  ___||  _  \\
  | |   | | | |  | |  | |__  | | | |
  | |   | | | |  | |  |  __| | | | |
 _| |_  | |/ /  _| |_ | |___ | |/ / 
 \\___/  |___/   \\___/ \\____/ |___/  "))

(defmethod collide ((player player) (guard guard) hit)
  (die player))

(define-handler (player attempt-takedown) (ev)
  (for:for ((entity over +world+))
    (when (and (typep entity 'guard)
               (not (eq :chase (state entity)))
               (< +takedown-min-distance+
                  (vdistance (location player) (location entity))
                  +takedown-max-distance+)
               (< (v. (angle-point (angle entity))
                      (nvunit (v- (location player) (location entity))))
                  +takedown-dot-threshold+))
      (v:info :player "Took down ~a" entity)
      (down entity)
      ;; You can only take down one at a time
      (for:end-for))))

(define-handler (player toggle-dragging) (ev)
  (if (dragged-entity player)
      (setf (dragged-entity player) nil)
      (for:for ((entity over +world+))
        (when (and (typep entity 'guard)
                   (eq :down (state entity))
                   (< (vdistance (location player) (location entity))
                      +dragging-max-distance+))
          (setf (dragged-entity player) entity)
          ;; You can only drag one guard
          (for:end-for)))))
