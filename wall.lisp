(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 unit) mesh
    (make-rectangle 1 1))

(define-shader-entity wall (vertex-entity located-entity solid)
  ((size :initarg :size :initform (vec 256 64) :accessor size)
   (vertex-array :initform (asset 'ld45 'unit))))

(defmethod paint :before ((wall wall) target)
  (scale-by (vx (size wall)) (vy (size wall)) 1))
