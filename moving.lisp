(in-package #:org.shirakumo.fraf.ld45)

(define-subject moving (located-entity axis-rotated-entity game-entity)
  ((axis :initform +vz+)
   (velocity :initform (vec 0 0) :accessor velocity)
   (bsize :initarg :bsize :initform (vec 16 16) :accessor bsize)))

(define-generic-handler (moving step tick))

(defmethod step ((moving moving) ev)
  (let ((loc (location moving))
        (vel (velocity moving)))
    (loop repeat 10
          for hit = (scan +world+ moving)
          while hit
          do (collide moving (hit-object hit) hit))
    (when (v/= vel 0)
      (setf (angle moving) (point-angle vel)))
    (nv+ loc vel)
    (vsetf vel 0 0)))

(defmethod scan ((wall wall) (moving moving))
  (aabb (location moving) (velocity moving)
        (location wall) (nv+ (v/ (size wall) 2) (bsize moving))))

(defmethod scan ((target moving) (source moving))
  (aabb (location source) (v+ (velocity source) (velocity target))
        (location target) (v+ (bsize source) (bsize target))))

(defmethod collide ((moving moving) (wall wall) hit)
  (let ((loc (location moving))
        (vel (velocity moving))
        (nor (hit-normal hit)))
    (nv+ loc (v* vel (hit-time hit)))
    (nv- vel (v* nor (v. vel nor)))))
