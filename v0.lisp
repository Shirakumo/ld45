(in-package #:org.shirakumo.fraf.ld45)

(defclass v0 (version)
  ())

(define-decoder (world v0) (metadata packet)
  (destructuring-bind (&key title author payload) metadata
    (let ((world (make-instance 'world :title title :author author :packet packet))
          (payload (packet-entry payload packet :element-type 'character)))
      (loop for (type . args) in (parse-sexps payload)
            for entity = (decode type args)
            do (enter entity world))
      world)))

(define-encoder (world v0) (_b packet)
  (with-packet-entry (stream "data" packet :element-type 'character)
    (for:for ((entity over world))
      (when (typep entity 'game-entity)
        (princ* (encode entity) stream))))
  (list :title (title world)
        :author (author world)
        :payload "data"))

(define-decoder (located-entity v0) (data _p)
  (destructuring-bind (&key location) data
    (make-instance (class-of located-entity) :location (decode 'vec2 location))))

(define-encoder (located-entity v0) (_b _p)
  `(,(type-of located-entity) :location ,(encode (location located-entity))))

(define-decoder (wall v0) (data _p)
  (destructuring-bind (&key size location) data
    (make-instance 'wall :location (decode 'vec2 location)
                         :size (decode 'vec2 size))))

(define-encoder (wall v0) (_b _p)
  `(wall :location ,(encode (location wall))
         :size ,(encode (size wall))))

(define-decoder (guard v0) (data _p)
  (destructuring-bind (&key location angle
                         route route-index route-direction end-action)
      data
    (make-instance 'guard
                   :location (decode 'vec2 location)
                   :angle angle
                   :route route
                   :route-index route-index
                   :route-direction route-direction
                   :end-action end-action)))

(define-encoder (guard v0) (_b _p)
  `(guard :location ,(encode (location guard))
          :angle ,(angle guard)
          :route ,(loop for node across (route guard)
                        collect (list (vx (location node))
                                      (vy (location node))
                                      (delay node)))
          :route-index ,(route-index guard)
          :route-direction ,(route-direction guard)
          :end-action ,(end-action guard)))

(define-decoder (ground v0) (data _p)
  (destructuring-bind (&key texture) data
    (make-instance (class-of ground) :texture (decode 'asset texture))))

(define-encoder (ground v0) (_b _p)
  `(,(type-of ground) :texture ,(encode (texture ground))))

(define-decoder (vec2 v0) (data _p)
  (destructuring-bind (x y) data
    (vec2 x y)))

(define-encoder (vec2 v0) (_b _p)
  (list (vx vec2)
        (vy vec2)))

(define-decoder (vec3 v0) (data _p)
  (destructuring-bind (x y z) data
    (vec3 x y z)))

(define-encoder (vec3 v0) (_b _p)
  (list (vx vec3)
        (vy vec3)
        (vz vec3)))

(define-decoder (vec4 v0) (data _p)
  (destructuring-bind (x y z w) data
    (vec4 x y z w)))

(define-encoder (vec4 v0) (_b _p)
  (list (vx vec4)
        (vy vec4)
        (vz vec4)
        (vw vec4)))

(define-decoder (asset v0) (data _p)
  (destructuring-bind (pool name) data
    (asset pool name)))

(define-encoder (asset v0) (_b _p)
  (list (name (pool asset))
        (name asset)))
