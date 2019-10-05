(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 player-mesh) mesh
    (make-sphere 16))

(define-global +move-speed+ 512)

(define-shader-subject player (vertex-entity moving)
  ((vertex-array :initform (asset 'ld45 'player-mesh))))

(defmethod step :before ((player player) ev)
  (let ((loc (location player))
        (vel (velocity player)))
    (loop for device in (cl-gamepad:devices)
          for move = (vec (cl-gamepad:axis device 0)
                          (cl-gamepad:axis device 1))
          do (when (< 0.3 (vlength move))
               (setf (vx move) (* (vx move) (cl-gamepad:axis-multiplier device 0)))
               (setf (vy move) (* (vy move) (cl-gamepad:axis-multiplier device 0)))
               (nv+ vel (nv* move +move-speed+ (dt ev)))))))
