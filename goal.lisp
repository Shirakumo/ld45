(in-package #:org.shirakumo.fraf.ld45)

(define-shader-subject goal (vertex-entity moving solid)
  ((vertex-array :initform (asset 'ld45 'unit))))

(defmethod collide ((player player) (goal goal) hit)
  (let* ((main (handler *context*))
         (next-level-name (second (member (current-level-name main)
                                          (level-names main)
                                          :test #'string=))))
    (when next-level-name
      (change-scene main (load-world (level-packet next-level-name)))
      (setf (current-level-name main) next-level-name))))

(defmethod collide ((goal goal) (guard guard) hit))

(defmethod paint :before ((goal goal) target)
  (scale-by (* 2 (vx (bsize goal))) (* 2 (vy (bsize goal))) 1))
