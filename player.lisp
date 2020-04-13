(in-package #:org.shirakumo.fraf.ld45)

(define-image player #p"player.png")
(defvar *capabilities* ())

(define-global +move-speed+ 200)
(define-global +dragging-move-speed+ 64)
(define-global +takedown-max-distance+ 40)
(define-global +takedown-dot-threshold+ -0.9)
(define-global +dragging-max-distance+ 24)
(define-global +bullet-speed+ 1024)
(define-global +bullet-cooldown-time+ 0.5)

(define-shader-subject player (human)
  ((name :initform :player)
   (texture :initform (asset 'ld45 'player))
   (move-timer :initform 0 :accessor move-timer)
   (capabilities :initarg :capabilities :initform () :accessor capabilities)
   (interactable :initform NIL :accessor interactable)
   (viewcone :initform (make-instance 'sector :aperture (->rad 45)) :reader viewcone))
  (:default-initargs
   :animations '((stand 0 1)
                 (walk 1 9)
                 (drag 9 17)
                 (shoot 17 18))))

(defmethod register-object-for-pass :after (pass (player player))
  (register-object-for-pass pass (viewcone player)))

(defmethod capable-of (thing (player (eql T)))
  (capable-of thing (unit :player +world+)))

(defmethod capable-of (thing (player player))
  (find thing (capabilities player)))

(defmethod contained-p ((point vec2) (player player))
  (let ((loc (location player))
        (half-size (bsize player)))
    (and (< (abs (- (vx loc) (vx point))) (vx half-size))
         (< (abs (- (vy loc) (vy point))) (vy half-size)))))

(defmethod step :before ((player player) ev)
  (let* ((vel (velocity player))
         (dt (dt ev)))
    (flet ((move (speed)
             (loop for device in (cl-gamepad:list-devices)
                   for move = (vec (cl-gamepad:axis device 0)
                                   (cl-gamepad:axis device 1))
                   do (when (< 0.3 (vlength move))
                        (setf (vx move) (* (vx move) (vx move) (signum (vx move))))
                        (setf (vy move) (* (vy move) (vy move) (signum (vy move))))
                        (nv+ vel (nv* move speed dt))))
             (when (retained 'movement :left)
               (decf (vx vel) (* dt 0.7 speed)))
             (when (retained 'movement :right)
               (incf (vx vel) (* dt 0.7 speed)))
             (when (retained 'movement :up)
               (incf (vy vel) (* dt 0.7 speed)))
             (when (retained 'movement :down)
               (decf (vy vel) (* dt 0.7 speed)))))
      (case (state player)
        (:aim
         (move 1)
         (when (v/= 0 vel)
           (setf (angle player) (point-angle vel))
           (vsetf vel 0 0)))
        (:shoot
         (decf (move-timer player) dt)
         (when (< (move-timer player) 0)
           (setf (state player) NIL)))
        (:dragging
         (move +dragging-move-speed+))
        (:dead)
        (T
         (move +move-speed+))))))

(defmethod step :after ((player player) ev)
  (unless (capable-of :clairvoyance player)
    (let ((dir (angle-point (angle player))))
      (setf (location (viewcone player)) (v- (location player) (v* dir 16)))
      (setf (direction (viewcone player)) dir)))
  (case (state player)
    (:dragging
     (setf (animation player) 'drag)
     (when (v= 0 (velocity player))
       (reset-animation player)))
    ((:shoot :aim)
     (setf (animation player) 'shoot))
    (T
     (if (v/= 0 (velocity player))
         (setf (animation player) 'walk)
         (setf (animation player) 'stand)))))

(defmethod die ((player player))
  (let ((screen (make-instance 'screen :texture (asset 'ld45 'game-over))))
    (pushnew :clairvoyance (capabilities player))
    (transition screen +world+)
    (enter screen +world+)))

(defmethod collide ((player player) (guard guard) hit)
  (when (eql :chase (state guard))
    (die player))
  NIL)

(define-handler (player attempt-takedown) (ev)
  (for:for ((entity over +world+))
    (when (and (typep entity 'guard)
               (not (eq :chase (state entity)))
               (< (vdistance (location player) (location entity))
                  +takedown-max-distance+)
               (< (v. (angle-point (angle entity))
                      (nvunit (v- (location player) (location entity))))
                  +takedown-dot-threshold+))
      (print :a)
      (down entity)
      ;; You can only take down one at a time
      (for:end-for))))

(define-handler (player toggle-dragging) (ev)
  (case (state player)
    (:dragging
     (setf (state player) NIL))
    ((NIL)
     (for:for ((entity over +world+))
       (when (and (draggable-p entity)
                  (< (vdistance (location player) (location entity))
                     +dragging-max-distance+))
         (drag entity player)
         ;; You can only drag one guard
         (for:end-for))))))

(define-handler (player continue-game) (ev)
  (when (eq :dead (state player))
    (v:info :player "Restarting...")
    (let ((main (handler *context*)))
      (change-scene main (load-world (level-packet (current-level-name main)))))))

(define-asset (ld45 bullet-mesh) mesh
    (make-rectangle 8 8))

(define-asset (ld45 bullet) image
    #p"bullet.png")

(define-shader-subject bullet (vertex-entity textured-entity moving)
  ((vertex-array :initform (asset 'ld45 'bullet-mesh))
   (texture :initform (asset 'ld45 'bullet))))

(defmethod step :before ((bullet bullet) ev)
  (setf (velocity bullet)
        (v* (angle-point (angle bullet))
            (* (dt ev) 0.7 +bullet-speed+))))

(defmethod collide ((bullet bullet) (guard guard) hit)
  (unless (eq (state guard) :down)
    (down guard)
    (leave bullet +world+)
    T))

(defmethod collide ((bullet bullet) (wall wall) hit)
  (leave bullet +world+)
  T)

(defmethod collide ((bullet bullet) thing hit)
  NIL)

(define-handler (player aim) (ev)
  (when (and (eql NIL (state player))
             (capable-of :shoot player))
    (setf (move-timer player) +bullet-cooldown-time+)
    (setf (state player) :aim)))

(define-handler (player shoot) (ev)
  (when (eql :aim (state player))
    (let ((bullet (make-instance 'bullet
                                 :location (vcopy (location player))
                                 :angle (angle player))))
      (transition bullet +world+)
      (enter bullet +world+))
    (setf (state player) :shoot)))

(define-handler (player interact) (ev)
  (when (interactable player)
    (pickup (interactable player) player)))
