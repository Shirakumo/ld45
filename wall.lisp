(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 unit) mesh
    (make-rectangle 1 1))

(define-shader-entity wall (vertex-entity located-entity solid)
  ((size :initform (vec 256 64) :accessor size)
   (bsize :initform (vec 128 32) :accessor bsize)
   (vertex-array :initform (asset 'ld45 'unit))))

(defmethod initialize-instance :after ((wall wall) &key size)
  (setf (size wall) (or size (size wall))))

(defmethod (setf size) :after (size (wall wall))
  (vsetf (bsize wall) (/ (vx size) 2) (/ (vy size) 2)))

(defmethod contained-p ((point vec2) (wall wall))
  (let ((loc (location wall))
        (half-size (v/ (size wall) 2)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod paint :before ((wall wall) target)
  (scale-by (vx (size wall)) (vy (size wall)) 1))
