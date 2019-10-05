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
