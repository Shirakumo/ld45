(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 pickup-mesh) mesh
    (make-rectangle 16 16))

(define-shader-entity pickup (vertex-entity textured-entity located-entity solid)
  ((vertex-array :initform (asset 'ld45 'pickup-mesh))))

(defmethod collide ((player player) (pickup pickup) hit)
  (setf (interactable player) pickup)
  NIL)

(defgeneric pickup (pickup player))

(define-image gun #p"gun.png")

(define-shader-entity gun (pickup)
  ((texture :initform (asset 'ld45 'gun))))

(defmethod pickup ((pickup gun) (player player))
  (pushnew :shoot (capabilities player)))

(define-image radar #p"radar.png")

(define-shader-entity radar (pickup)
  ((texture :initform (asset 'ld45 'radar))))

(defmethod pickup ((pickup radar) (player player))
  (pushnew :clairvoyance (capabilities player)))

(define-image bionic-eye #p"bionic-eye.png")

(define-shader-entity bionic-eye (pickup)
  ((texture :initform (asset 'ld45 'bionic-eye))))

(defmethod pickup ((pickup bionic-eye) (player player))
  (pushnew :line-of-sight (capabilities player)))
