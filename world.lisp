(in-package #:org.shirakumo.fraf.ld45)

(define-global +world+ NIL)

(defclass world (pipelined-scene)
  ())

(defmethod unit (name (_ (eql T)))
  (unit name +world+))

(defclass empty-world (world)
  ())

(defmethod initialize-instance :after ((world empty-world) &key)
  (enter (make-instance 'player) world)
  (change-class world 'world))

(defun load-world (meta-file)
  )
