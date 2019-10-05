(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 player-mesh) mesh
    (make-triangle 32 32 :orientation :right))

(define-global +move-speed+ 512)

(define-shader-subject player (vertex-entity moving)
  ((name :initform :player)
   (vertex-array :initform (asset 'ld45 'player-mesh))))

(defmethod contained-p ((point vec2) (player player))
  (let ((loc (location player))
        (half-size (bsize player)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod step :before ((player player) ev)
  (let ((loc (location player))
        (vel (velocity player))
        (dt (dt ev)))
    (loop for device in (cl-gamepad:devices)
          for move = (vec (cl-gamepad:axis device 0)
                          (cl-gamepad:axis device 1))
          do (when (< 0.3 (vlength move))
               (setf (vx move) (* (vx move) (vx move) (signum (vx move)) (cl-gamepad:axis-multiplier device 0)))
               (setf (vy move) (* (vy move) (vy move) (signum (vy move)) (cl-gamepad:axis-multiplier device 1)))
               (nv+ vel (nv* move +move-speed+ dt))))
    (when (retained 'movement :left)
      (decf (vx vel) (* dt 0.7 +move-speed+)))
    (when (retained 'movement :right)
      (incf (vx vel) (* dt 0.7 +move-speed+)))
    (when (retained 'movement :up)
      (incf (vy vel) (* dt 0.7 +move-speed+)))
    (when (retained 'movement :down)
      (decf (vy vel) (* dt 0.7 +move-speed+)))))

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
