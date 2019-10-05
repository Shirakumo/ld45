(in-package #:org.shirakumo.fraf.ld45)

(defparameter +grid-size+ 32)

(define-subject editor (located-entity)
  ((name :initform :editor)
   (entity :initform nil :accessor entity)
   (mode :initform :select :accessor mode)
   (start-location :initform nil :accessor start-location)))

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
       (cond
         ((and (eq button :left) (typep entity 'located-entity))
          (setf (entity active-editor) entity)
          (setf (mode active-editor) :moving)))))
    (:delete
     (let ((entity (entity-at-point (world-location pos) +world+)))
       (leave entity *scene*)
       (setf (mode active-editor) :select)))
    (:place-wall
     (let* ((location (nvalign (world-location pos) +grid-size+))
            (wall (make-instance 'wall
                                 :location location
                                 :size (vec2 0 0))))
       (setf (start-location active-editor) location)
       (enter wall *scene*)
       (setf (entity active-editor) wall)
       (setf (mode active-editor) :placing-wall)))
    (:place-player
     (let* ((location (nvalign (world-location pos) +grid-size+))
            (player (make-instance 'player
                                   :location location)))
       (enter player *scene*)
       (setf (mode active-editor) :select)))
    (:place-guard
     (let* ((location (nvalign (world-location pos) +grid-size+))
            (guard (make-instance 'guard
                                  :location location)))
       (enter guard *scene*)
       (setf (mode active-editor) :select)))))

(define-handler (active-editor mouse-release) (ev pos button)
  (case (mode active-editor)
    (:moving
     (setf (mode active-editor) :select))
    (:placing-wall
     (let* ((entity (entity active-editor))
            (size (size entity)))
       (when (or (zerop (vx size))
                 (zerop (vy size)))
         (leave entity *scene*)))
     (setf (mode active-editor) :select))))

(define-handler (active-editor mouse-move) (ev pos)
  (case (mode active-editor)
    (:placing-wall
     (let* ((entity (entity active-editor))
            (location (nvalign (world-location pos) +grid-size+))
            (start-location (start-location active-editor))
            (size (v- location start-location)))
       (setf (size entity) (vabs size))
       (setf (location entity) (v- location (v/ size 2)))))
    (:moving
     (let* ((entity (entity active-editor))
            (bsize (bsize entity)))
       (setf (location (entity active-editor))
             (v+ (nvalign (v- (world-location pos) bsize) +grid-size+)
                 bsize))))))
