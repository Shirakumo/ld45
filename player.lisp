(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 player-mesh) mesh
    (make-sphere 16))

(define-shader-subject player (vertex-entity located-entity axis-rotated-entity)
  ((axis :initform +vz+)
   (vertex-array :initform (asset 'ld45 'player-mesh))))
