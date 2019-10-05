(in-package #:org.shirakumo.fraf.ld45)

(defclass main (trial:main)
  ((scene :initform NIL))
  (:default-initargs
   :title "LD45"
   :width 1280
   :height 720))

(defmethod initialize-instance ((main main) &key world)
  (call-next-method)
  (setf (scene main)
        (if world
            (load-world world)
            (make-instance 'empty-world))))

(defmethod (setf scene) :after (scene (main main))
  (setf +world+ scene))

(defmethod finalize :after ((main main))
  (setf +world+ NIL))

(defmethod setup-rendering :after ((main main))
  (disable :cull-face :scissor-test :depth-test))

(defmethod setup-scene ((main main) scene)
  (enter (make-instance 'inactive-editor) scene)
  (enter (make-instance 'camera) scene)
  (enter (make-instance 'render-pass) scene))

(defun launch ()
  (trial:launch 'main))
