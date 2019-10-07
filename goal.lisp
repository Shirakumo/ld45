(in-package #:org.shirakumo.fraf.ld45)

(define-shader-subject goal (vertex-entity moving solid)
  ((vertex-array :initform (asset 'ld45 'unit))
   (reset :initform NIL :initarg :reset :accessor reset)))

(defmethod collide ((player player) (goal goal) hit)
  (let* ((main (handler *context*))
         (next-level-name (second (member (current-level-name main)
                                          (level-names main)
                                          :test #'string=))))
    (if (reset goal)
        (setf *capabilities* NIL)
        (setf *capabilities* (capabilities player)))
    (when next-level-name
      (change-scene main (load-world (level-packet next-level-name)))
      (setf (current-level-name main) next-level-name))))

(defmethod collide ((goal goal) (guard guard) hit))

(defmethod paint :around ((goal goal) target)
  (when (active-p (unit :editor +world+))
    (call-next-method)))

(defmethod paint :before ((goal goal) target)
  (scale-by (* 2 (vx (bsize goal))) (* 2 (vy (bsize goal))) 1))
