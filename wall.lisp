(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 unit) mesh
    (make-rectangle 1 1))

(define-shader-entity wall (vertex-entity located-entity solid)
  ((size :initarg :size :initform (vec 256 64) :accessor size)
   (bsize :initform (vec 128 32) :accessor bsize)
   (vertex-array :initform (asset 'ld45 'unit))))

(defmethod (setf size) :after (size (wall wall))
  (vsetf (bsize wall) (/ (vx size) 2) (/ (vy size) 2)))

(defmethod paint :before ((wall wall) target)
  (scale-by (vx (size wall)) (vy (size wall)) 1))
