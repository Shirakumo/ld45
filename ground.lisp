(in-package #:org.shirakumo.fraf.ld45)

(define-image intro #p"intro.png")
(define-image map1 #p"map1.png")
(define-image map2 #p"map2.png")
(define-image map3 #p"map3.png")

(define-asset (ld45 map-mesh) mesh
    (make-rectangle (* 32 32) (* 32 32)))

(define-shader-entity ground (vertex-entity textured-entity game-entity)
  ((vertex-array :initform (asset 'ld45 'map-mesh))))

(defmethod paint :around ((ground ground) target)
  (call-next-method))
