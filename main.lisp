(in-package #:org.shirakumo.fraf.ld45)

(defclass main (trial:main)
  ((scene :initform NIL)
   (level-names :initform () :accessor level-names)
   (current-level-name :initform nil :accessor current-level-name))
  (:default-initargs
   :clear-color (vec4 0 0 0 0)
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
  (disable :cull-face :scissor-test :depth-test :stencil-test))

(defmethod setup-scene ((main main) scene)
  (enter (make-instance 'inactive-editor) scene)
  (enter (make-instance 'camera) scene)
  (let ((fog-of-war (make-instance 'fog-of-war))
        (blur-pass (make-instance 'radial-blur-pass :uniforms `(("strength" 0.2)
                                                                ("samples" 36))))
        (light (make-instance 'lighting-pass)))
    (connect (port fog-of-war 'color) (port blur-pass 'previous-pass) scene)
    (connect (port blur-pass 'color) (port light 'fog) scene)))

(defun launch (&optional world)
  (trial:launch 'main :world world :level-names '("map1/" "map2/" "map3/")))
