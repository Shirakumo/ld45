(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 guard) image
    #p"guard.png"
  :min-filter :nearest
  :mag-filter :nearest)

(defclass route-node ()
  ((location :initarg :location :initform (vec 0 0) :accessor location)
   (delay :initarg :delay :initform 0. :accessor delay)))

(defmethod print-object ((node route-node) stream)
  (format stream "~s" (list 'route-node (location node) (delay node))))

(defun route-node (location delay)
  (make-instance 'route-node :location location :delay delay))

(define-global +guard-sway-aperture+ (->rad 30))
(define-global +guard-patrol-speed+ 64)
(define-global +guard-chase-speed+ 400)
(define-global +guard-scan-time+ 5)
(define-global +guard-down-time+ 10)

(define-shader-subject guard (draggable solid human)
  ((texture :initform (asset 'ld45 'guard))
   (viewcone :initform (make-instance 'sector) :reader viewcone)
   (state :initform :return)
   (route :initform (make-array 0 :adjustable T :fill-pointer T) :accessor route)
   (route-index :initarg :route-index :initform 0 :accessor route-index)
   (route-direction :initarg :route-direction :initform 1 :accessor route-direction)
   (end-action :initarg :end-action :initform :loop :accessor end-action)
   (chase-path :initform NIL :accessor chase-path)
   (look-timer :initform 0 :accessor look-timer)
   (down-timer :initform 0 :accessor down-timer))
  (:default-initargs
   :animations '((stand 0 1)
                 (walk 1 9)
                 (down 9 14 :loop-to 13))))

(defmethod initialize-instance :after ((guard guard) &key route)
  (loop for (x y d) in route
        do (vector-push-extend (route-node (vec x y) d) (route guard))))

(defmethod collide ((guard guard) (other guard) hit))

(defmethod draggable-p ((guard guard))
  (eq :down (state guard)))

(defmethod contained-p ((point vec2) (guard guard))
  (let ((loc (location guard))
        (half-size (bsize guard)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod enter :after ((guard guard) (scene scene))
  (update-scene-cache (viewcone guard) scene))

(defmethod update-scene-cache ((guard guard) (scene scene))
  (update-scene-cache (viewcone guard) scene))

(defmethod register-object-for-pass :after (pass (guard guard))
  (register-object-for-pass pass (viewcone guard)))

(defmethod paint :around ((guard guard) pass)
  (unless (eql :down (state guard))
    (paint (viewcone guard) pass))
  (call-next-method))

(defmethod (setf route-index) :after (index (guard guard))
  (when (and (/= 0 (length (route guard)))
             (not (<= 0 index (1- (length (route guard))))))
    (ecase (end-action guard)
      (:loop
         (setf (route-index guard) 0))
      (:reverse
       (setf (route-direction guard) (* -1 (route-direction guard)))
       (incf (route-index guard) (route-direction guard))))))

(defmethod move-towards ((guard guard) target speed)
  (when (< speed (vsqrdist2 (location guard) target))
    (let ((dir (nvunit (v- target (location guard)))))
      (nv+ (velocity guard) (nv* dir speed)))))

(defmethod step :before ((guard guard) ev)
  (let ((vel (velocity guard))
        (route (route guard))
        (dt (dt ev)))
    (case (state guard)
      (:return
        (setf (state guard) (if (/= 0 (length route)) :patrol :look)))
      (:patrol
       (unless (move-towards guard (location (aref route (route-index guard)))
                             (* dt +guard-patrol-speed+))
         (setf (look-timer guard) (delay (aref route (route-index guard))))
         (setf (state guard) :look))
       (when (visible-p (location (unit :player T)) (viewcone guard))
         (chase (unit :player T) guard)))
      (:look
       (decf (look-timer guard) dt)
       (let* ((progress (* (/ (mod (look-timer guard) +guard-scan-time+) +guard-scan-time+) PIF 2))
              (angle (+ (angle guard) (* +guard-sway-aperture+ (sin progress)))))
         (setf (direction (viewcone guard))  (angle-point angle)))
       (when (< (look-timer guard) 0)
         (when (/= 0 (length route))
           (incf (route-index guard) (route-direction guard)))
         (setf (state guard) :return))
       (when (visible-p (location (unit :player T)) (viewcone guard))
         (chase (unit :player T) guard)))
      (:chase
       (cond ((null (chase-path guard))
              (setf (look-timer guard) +guard-scan-time+)
              (setf (state guard) :look))
             ((not (move-towards guard (first (chase-path guard))
                                 (* dt +guard-chase-speed+)))
              (when (visible-p (location (unit :player T)) (viewcone guard))
                (chase (unit :player T) guard))
              (pop (chase-path guard)))))
      (:down
       (decf (down-timer guard) dt)
       (when (<= (down-timer guard) 0)
         (when (dragger guard)
           (setf (dragger guard) NIL))
         (setf (state guard) :return))))
    (when (v/= 0 vel)
      (setf (direction (viewcone guard)) (vunit vel)))))

(defmethod step :after ((guard guard) ev)
  (setf (location (viewcone guard)) (location guard))
  (case (state guard)
    (:down
     (setf (animation guard) 'down))
    ((:patrol :chase)
     (setf (animation guard) 'walk))
    (T
     (setf (animation guard) 'stand))))

(defmethod down ((guard guard))
  (unless (eql :down (state guard))
    (setf (state guard) :down)
    (setf (down-timer guard) +guard-down-time+)))

(defmethod chase ((target located-entity) (guard guard))
  (chase (location target) guard))

(defmethod chase ((target vec2) (guard guard))
  (setf (chase-path guard) (nreverse (find-path (path-map +world+) (location guard) target)))
  (setf (state guard) :chase))
