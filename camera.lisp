(in-package #:org.shirakumo.fraf.ld45)

(define-subject camera (trial:2d-camera)
  ((name :initform :camera)
   (zoom :initarg :zoom :initform 1.0 :accessor zoom)
   (view-scale :initform 1.0 :accessor view-scale)
   (target-size :initarg :target-size :initform (vec 1280 720) :accessor target-size)
   (target :initarg :target :initform NIL :accessor target)
   (intended-location :initform (vec 0 0) :accessor intended-location)
   (shake-counter :initform 0 :accessor shake-counter)
   (shake-intensity :initform 3 :accessor shake-intensity))
  (:default-initargs :location (vec 0 0)))

(defmethod enter :after ((camera camera) (scene scene))
  (setf (target camera) (unit :player scene)))

(define-handler (camera trial:tick) (ev tt)
  (let ((loc (location camera))
        (int (intended-location camera)))
    (unless (active-p (unit :editor T))
      (when (target camera)
        (let ((tar (location (target camera))))
          (vsetf int (vx tar) (vy tar))))
      ;; Smooth camera movement
      (let* ((dir (v- int loc))
             (len (max 1 (vlength dir)))
             (ease (clamp 0 (+ 0.2 (/ (expt len 1.5) 100)) 20)))
        (nv* dir (/ ease len))
        (nv+ loc dir)))
    (when (< 0 (shake-counter camera))
      (decf (shake-counter camera))
      (nv+ loc (vrandr (* (shake-intensity camera) 0.1) (shake-intensity camera))))))

(defmethod (setf zoom) :after (zoom (camera camera))
  (setf (view-scale camera) (* (float (/ (width *context*) (* 2 (vx (target-size camera)))))
                               (zoom camera))))

(define-handler (camera resize) (ev)
  (setf (view-scale camera) (* (float (/ (width ev) (* 2 (vx (target-size camera)))))
                               (zoom camera)))
  (setf (vy (target-size camera)) (/ (height ev) (view-scale camera) 2)))

(defmethod project-view ((camera camera) ev)
  (let* ((z (view-scale camera))
         (v (nv- (v/ (target-size camera) (zoom camera)) (location camera))))
    (reset-matrix *view-matrix*)
    (scale-by z z z *view-matrix*)
    (translate-by (vx v) (vy v) 100 *view-matrix*)))

(defun shake-camera (&key (duration 20) (intensity 3))
  (let ((camera (unit :camera +world+)))
    (setf (shake-counter camera) duration)
    (setf (shake-intensity camera) intensity)))
