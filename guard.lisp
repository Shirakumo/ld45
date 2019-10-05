(in-package #:org.shirakumo.fraf.ld45)

(defclass route-node ()
  ((location :initarg :location :initform (vec 0 0) :accessor location)
   (delay :initarg :delay :initform 0. :accessor delay)))

(defmethod print-object ((node route-node) stream)
  (format stream "~s" (list 'route-node (location node) (delay node))))

(defun route-node (location delay)
  (make-instance 'route-node :location location :delay delay))

(define-asset (ld45 guard-mesh) mesh
    (make-triangle 32 16 :orientation :right))

(define-global +guard-patrol-speed+ 256)
(define-global +guard-scan-time+ 1)

(define-shader-subject guard (vertex-entity moving solid)
  ((vertex-array :initform (asset 'ld45 'guard-mesh))
   (state :initform :patrol)
   (route :initform (make-array 0 :adjustable T :fill-pointer T) :accessor route)
   (route-index :initarg :route-index :initform 0 :accessor route-index)
   (route-direction :initarg :route-direction :initform 1 :accessor route-direction)
   (next-node-timer :initform 0 :accessor next-node-timer)
   (end-action :initarg :end-action :initform :loop :accessor end-action)))

(defmethod initialize-instance :after ((guard guard) &key route)
  (loop for (x y d) in route
        do (vector-push-extend (route-node (vec x y) d) (route guard)))
  (when (= 0 (length route))
    (setf (state guard) NIL)))

(defmethod collide ((guard guard) (other guard) hit))

(defmethod (setf route-index) :after (index (guard guard))
  (when (and (/= 0 (length (route guard)))
             (not (<= 0 index (1- (length (route guard))))))
    (ecase (end-action guard)
      (:loop
         (setf (route-index guard) 0))
      (:reverse
       (setf (route-direction guard) (* -1 (route-direction guard)))
       (incf (route-index guard) (route-direction guard))))))

(defmethod step :before ((guard guard) ev)
  (let ((route (route guard))
        (dt (dt ev)))
    (case (state guard)
      (:patrol
       (cond ((< (vsqrdist2 (location guard) (location (aref route (route-index guard))))
                 (* dt +guard-patrol-speed+))
              (setf (next-node-timer guard) (delay (aref route (route-index guard))))
              (setf (state guard) :wait))
             (T
              (let ((dir (nvunit (v- (location (aref route (route-index guard))) (location guard)))))
                (nv+ (velocity guard) (nv* dir dt +guard-patrol-speed+))))))
      (:wait
       (decf (next-node-timer guard) dt)
       (when (< (next-node-timer guard) 0)
         (incf (route-index guard) (route-direction guard))
         (setf (state guard) :patrol)))
      (:chase))))
