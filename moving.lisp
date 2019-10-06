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
          for hits = (scan +world+ moving)
          while hits
          do (loop for hit in hits
                   until (collide moving (hit-object hit) hit)))
    (when (v/= vel 0)
      (setf (angle moving)
            (case (state moving)
              (:dragging (- (point-angle vel) PI))
              (t (point-angle vel)))))
    (nv+ loc vel)))

(defmethod step :around ((moving moving) ev)
  (vsetf (velocity moving) 0 0)
  (call-next-method))

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
    (nv- vel (v* nor (v. vel nor)))
    T))

(define-subject draggable (moving)
  ((dragger :initform NIL :accessor dragger)))

(defmethod draggable-p ((draggable draggable)) T)
(defmethod draggable-p ((entity entity)) NIL)

(defmethod step :after ((entity draggable) ev)
  (let ((d (dragger entity)))
    (when d
      (setf (location entity) (nv+ (nv* (angle-point (- (angle d) PI)) (bsize entity) -1.5) (location d)))
      (setf (angle entity) (angle d))
      (unless (eq :dragging (state d))
        (setf (dragger entity) NIL)))))

(defmethod (setf dragger) :before ((nobody null) (draggable draggable))
  (when (eq :dragging (state (dragger draggable)))
    (setf (state (dragger draggable)) NIL)))

(defmethod drag ((draggable draggable) (dragger moving))
  (setf (dragger draggable) dragger)
  (setf (state dragger) :dragging))

(define-asset (ld45 human) mesh
    (make-rectangle 64 64))

(define-shader-subject human (moving animated-sprite-subject)
  ((vertex-array :initform (asset 'ld45 'human))
   (size :initform (vec 32 32))
   (bsize :initform (vec 32 32))))
