(in-package #:org.shirakumo.fraf.ld45)

(define-pool ld45
  :base :ld45)

(defmacro define-global (name value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if (boundp ',name)
         (setf ,name ,value)
         #+sbcl (sb-ext:defglobal ,name ,value)
         #-sbcl (defvar ,name ,value))))

(defun clamp (low mid high)
  (max low (min mid high)))

(defun ->rad (deg)
  (* PI (/ deg 180)))

(defun ->deg (rad)
  (/ (* rad 180) PI))

(defun vrand (min max)
  (vec (+ min (random (- max min)))
       (+ min (random (- max min)))))

(defun vrandr (min max)
  (let ((r (+ min (random (- max min))))
        (phi (random (* 2 PI))))
    (vec (* r (cos phi))
         (* r (sin phi)))))

(defun vsqrdist2 (a b)
  (declare (type vec2 a b))
  (declare (optimize speed))
  (+ (expt (- (vx2 a) (vx2 b)) 2)
     (expt (- (vy2 a) (vy2 b)) 2)))

(defclass solid () ())

(defclass located-entity (entity)
  ((location :initarg :location :initform (vec 0 0) :accessor location
             :type vec2 :documentation "The location in 2D space.")))

(defmethod print-object ((entity located-entity) stream)
  (print-unreadable-object (entity stream :type T :identity T)
    (format stream "~a" (location entity))))

(defmethod paint :around ((obj located-entity) target)
  (with-pushed-matrix ()
    (translate-by (round (vx (location obj))) (round (vy (location obj))) 0)
    (call-next-method)))
