(in-package #:org.shirakumo.fraf.ld45)

(defparameter +grid-size+ 32)

(define-subject editor (located-entity)
  ((name :initform :editor)
   (entity :initform nil :accessor entity)
   (mode :initform :select :accessor mode)
   (vel :initform (vec 0 0) :accessor vel)
   (start-location :initform nil :accessor start-location)
   (visible-path :initform nil :accessor visible-path)))

(define-subject inactive-editor (editor)
  ())

(defmethod active-p ((_ inactive-editor)) NIL)

(define-handler (inactive-editor toggle-editor) (ev)
  (change-class inactive-editor 'active-editor))

(define-subject active-editor (editor)
  ())

(defmethod active-p ((_ active-editor)) T)

(define-handler (active-editor toggle-editor) (ev)
  (change-class active-editor 'inactive-editor))

(define-handler (active-editor save-world) (ev)
  (unless (packet +world+)
    (with-packet (packet (merge-pathnames
                          (query "Please enter the world save path" :parse #'uiop:native-namestring)
                          (pool-path 'ld45 NIL))
                         :direction :input)
      (setf (packet +world+) packet)))
  (save-world +world+ T :version T))

(define-handler (active-editor load-world) (ev)
  (change-scene (handler *context*) (load-world +world+)))

;;; Mode switching

(define-handler (active-editor select-entity) (ev)
  (setf (mode active-editor) :select))

(define-handler (active-editor delete-entity) (ev)
  (setf (mode active-editor) :delete))

(define-handler (active-editor place-wall) (ev)
  (setf (mode active-editor) :place-wall))

(define-handler (active-editor place-player) (ev)
  (setf (mode active-editor) :place-player))

(define-handler (active-editor place-guard) (ev)
  (setf (mode active-editor) :place-guard))

;;; Entity manipulation

(defgeneric contained-p (point thing)
  (:method (point thing) nil))

(defun world-location (screen-position)
  (let ((camera (unit :camera t))
        (loc (vcopy screen-position)))
    (prog1 loc
      (nv+ (nv/ loc (view-scale camera)) (location camera))
      (nv- loc (v/ (target-size camera) (zoom camera))))))

(defun entity-at-point (point world)
  (for:for ((entity over world))
    (when (contained-p point entity)
      (return entity))))

(define-handler (active-editor mouse-press) (ev pos button)
  (case (mode active-editor)
    (:select
     (let ((entity (entity-at-point (world-location pos) +world+)))
       (if (typep entity 'guard)
           (show-path active-editor entity)
           (hide-path active-editor))
       (cond
         ((and (eq button :left) (typep entity 'located-entity))
          (setf (entity active-editor) entity)
          (setf (mode active-editor) :moving)))))
    (:delete
     (let ((entity (entity-at-point (world-location pos) +world+)))
       (leave entity +world+)
       (setf (mode active-editor) :select)
       (update-scene-cache +world+ +world+)))
    (:place-wall
     (let* ((location (nvalign (world-location pos) +grid-size+))
            (wall (make-instance 'wall
                                 :location location
                                 :size (vec2 0 0))))
       (setf (start-location active-editor) location)
       (enter wall +world+)
       (setf (entity active-editor) wall)
       (setf (mode active-editor) :placing-wall)))
    (:place-player
     (let* ((location (nvalign (world-location pos) +grid-size+))
            (player (make-instance 'player
                                   :location location)))
       (enter player +world+)
       (setf (mode active-editor) :select)))
    (:place-guard
     (handle-place-guard-press active-editor pos button))
    (:placing-guard
     (handle-placing-guard-press active-editor pos button))))

(define-handler (active-editor mouse-release) (ev pos button)
  (case (mode active-editor)
    (:moving
     (setf (mode active-editor) :select))
    (:placing-wall
     (let* ((entity (entity active-editor))
            (size (size entity)))
       (when (or (zerop (vx size))
                 (zerop (vy size)))
         (leave entity +world+)
         (update-scene-cache +world+ +world+)))
     (setf (mode active-editor) :select))))

(define-handler (active-editor mouse-move) (ev pos)
  (case (mode active-editor)
    (:placing-wall
     (let* ((entity (entity active-editor))
            (location (nvalign (world-location pos) +grid-size+))
            (start-location (start-location active-editor))
            (size (v- location start-location)))
       (setf (size entity) (vabs size))
       (setf (location entity) (v- location (v/ size 2)))
       (update-scene-cache +world+ +world+)))
    (:moving
     (let* ((entity (entity active-editor))
            (bsize (bsize entity)))
       (setf (location (entity active-editor))
             (v+ (nvalign (v- (world-location pos) bsize) +grid-size+)
                 bsize)))
     (update-scene-cache +world+ +world+))))

(define-handler (active-editor mouse-scroll) (ev delta)
  (when (or (retained 'key :control)
            (retained 'key :left-control) (retained 'key :control-l)
            (retained 'key :right-control) (retained 'key :control-r))
    (handle-editor-zoom delta))
  (when (eq :placing-guard (mode active-editor))
    (handle-modify-guard-delay active-editor (if (< 0 delta) 1 -1))))

(define-handler (active-editor tick) (ev)
  (update-visible-path active-editor)
  (update-editor-location active-editor))

;;; Placing guards

(defun handle-place-guard-press (editor pos button)
  (declare (ignore button))
  (let* ((location (nvalign (world-location pos) +grid-size+))
         (guard (make-instance 'guard :location location)))
    (transition guard +world+)
    (enter guard +world+)
    (setf (entity editor) guard)
    (setf (mode editor) :placing-guard)))

(defun handle-placing-guard-press (editor pos button)
  (let ((location (nvalign (world-location pos) +grid-size+))
        (guard (entity editor)))
    (case button
      (:left
       (vector-push-extend (route-node location 0) (route guard))
       (show-path editor guard))
      (:right
       (when (plusp (length (route guard)))
         (vector-pop (route guard)))))))

(defun handle-modify-guard-delay (editor change)
  (let* ((route (route (entity editor))))
    (when (plusp (length route))
      (let ((node (elt route (1- (fill-pointer route)))))
        (setf (delay node) (max 0 (+ change (delay node))))))))

(define-handler (active-editor loop-guard-path) (ev)
  (when (eq :placing-guard (mode active-editor))
    (setf (end-action (entity active-editor)) :loop)))

(define-handler (active-editor reverse-guard-path) (ev)
  (when (eq :placing-guard (mode active-editor))
    (setf (end-action (entity active-editor)) :reverse)))

(define-handler (active-editor finish-guard-path) (ev)
  (when (eq :placing-guard (mode active-editor))
    (let ((guard (entity active-editor)))
      (setf (mode active-editor) :select)
      (when (plusp (length (route guard)))
        (setf (state guard) :patrol)))))

;;; Zooming and moving the camera

(defun handle-editor-zoom (delta)
  (let ((camera (unit :camera +world+)))
    (setf (zoom camera) (* (zoom camera) (if (< 0 delta) 1.5 (/ 1.5))))))

(defun update-editor-location (editor)
  (let ((speed (/ 20 (zoom (unit :camera +world+)))))
    (cond ((retained 'movement :left) (setf (vx (vel editor)) (- speed)))
          ((retained 'movement :right) (setf (vx (vel editor)) (+ speed)))
          (T (setf (vx (vel editor)) 0)))
    (cond ((retained 'movement :up) (setf (vy (vel editor)) (+ speed)))
          ((retained 'movement :down) (setf (vy (vel editor)) (- speed)))
          (T (setf (vy (vel editor)) 0))))
  (nv+ (location editor) (vel editor))
  (setf (location (unit :camera +world+)) (location editor)))

;;; Route visualization

(define-shader-entity visible-path (vertex-entity)
  ((mesh :initform nil :accessor mesh)))

(defmethod (setf mesh) :after (mesh (entity visible-path))
  (if (slot-boundp entity 'vertex-array)
      (let ((vbo (car (second (bindings (vertex-array entity)))))
            (ebo (first (bindings (vertex-array entity)))))
        (trial::replace-vertex-data (buffer-data vbo) mesh)
        (setf (buffer-data ebo) (faces mesh))
        (trial:resize-buffer vbo (* (length (buffer-data vbo)) (gl-type-size :float))
                             :data (buffer-data vbo))
        (trial:resize-buffer ebo (* (length (buffer-data ebo)) (gl-type-size :float))
                             :data (buffer-data ebo))
        (setf (size (vertex-array entity)) (length (faces mesh))))
      (setf (vertex-array entity) (change-class mesh 'vertex-array))))

(defun route-vertices (guard)
  (with-vertex-filling ((make-instance 'vertex-mesh :vertex-type 'colored-vertex :face-length 2))
    (let ((route (route guard))
          (route-index (route-index guard)))
      (loop for node across route
         for previous-location = nil then location
         for location = (location node)
         for i from 0
         when previous-location
         do (vertex :position (vxy_ previous-location)
                    :color (vec 1 0 0 1))
         and do (vertex :position (vxy_ location)
                        :color (vec 1 0 0 1))
         ;; Render a line proportionate to the delay
         do (loop
               repeat (delay node)
               for i from 1
               do (vertex :position (vxy_ (v+ location (vec 0 (* 5 (- i)))))
                          :color (vec 1 1 1 1))
                 (vertex :position (vxy_ (v+ location (vec 7 (* 5 (- i)))))
                         :color (vec 1 1 1 1))))
      ;; Close the loop if necessary
      (when (and (eq :loop (end-action guard))
                 (< 2 (length route)))
        (vertex :position (vxy_ (location (elt route (1- (length route)))))
                :color (vec 0 0 1 1))
        (vertex :position (vxy_ (location (elt route 0)))
                :color (vec 0 0 1 1)))
      ;; Draw a line from the guard to its current destination
      (when (< route-index (length route))
        (vertex :position (vxy_ (location guard))
                :color (vec 0 1 0 1))
        (vertex :position (vxy_ (vxy_ (location (elt route route-index))))
                :color (vec 0 1 0 1))))))

(defun show-path (editor guard)
  (when (plusp (length (route guard)))
    (if (visible-path editor)
        (setf (mesh (visible-path editor)) (route-vertices guard))
        (let ((visible-path (make-instance 'visible-path)))
          (setf (mesh visible-path) (route-vertices guard))
          (transition visible-path +world+)
          (enter visible-path +world+)
          (setf (visible-path editor) visible-path)))))

(defun hide-path (editor)
  (when (visible-path editor)
    (leave (visible-path editor) +world+)
    (setf (visible-path editor) nil)))

(defun update-visible-path (editor)
  (when (and (visible-path editor)
             (typep (entity editor) 'guard))
    (setf (mesh (visible-path editor)) (route-vertices (entity editor)))))
