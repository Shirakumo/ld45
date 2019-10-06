(in-package #:org.shirakumo.fraf.ld45)

(defclass main (trial:main)
  ((scene :initform NIL)
   (level-names :initform nil :accessor level-names)
   (current-level-name :initform nil :accessor current-level-name))
  (:default-initargs
   :title "LD45"
   :width 1280
   :height 720))

(defun level-packet (level-name)
  (with-packet (packet (merge-pathnames (uiop:parse-unix-namestring level-name)
                                        (pool-path 'ld45 nil))
                       :direction :input)
    packet))

(defmethod initialize-instance ((main main) &key world level-names)
  (call-next-method)
  (when level-names
    (let ((first-level-name (first level-names)))
      (setf (level-names main) level-names)
      (setf (current-level-name main) first-level-name)
      (setf world (level-packet first-level-name))))
  (setf (scene main)
        (cond
          (world (load-world world))
          (t (make-instance 'empty-world)))))

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

(defun launch (&optional world)
  (trial:launch 'main :world world))
