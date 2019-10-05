(in-package #:org.shirakumo.fraf.ld45)

(define-global +world+ NIL)

(defclass world (pipelined-scene)
  ())

(defmethod unit (name (_ (eql T)))
  (unit name +world+))

(defmethod scan ((world world) (target located-entity))
  (let ((result NIL)
        (loc (location target)))
    (do-container-tree (entity world result)
      (when (and (not (eq entity target))
                 (typep entity 'solid))
        (let ((hit (scan entity target)))
          (when (and hit
                     (or (null result)
                         (< (vsqrdist2 loc (hit-location hit))
                            (vsqrdist2 loc (hit-location result)))))
            (setf (hit-object hit) entity)
            (setf result hit)))))))

(defclass empty-world (world)
  ())

(defmethod initialize-instance :after ((world empty-world) &key)
  (enter (make-instance 'wall :location (vec 0 64)) world)
  (enter (make-instance 'player) world)
  (change-class world 'world))

(defun load-world (meta-file)
  )
