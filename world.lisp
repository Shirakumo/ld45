(in-package #:org.shirakumo.fraf.ld45)

(define-global +world+ NIL)

(defclass world (pipelined-scene)
  ((packet :initarg :packet :accessor packet)
   (title :initform "Untitled" :initarg :title :accessor title)
   (author :initform "Anonymous" :initarg :author :accessor author)))

(defmethod unit (name (_ (eql T)))
  (unit name +world+))

(defmethod scan ((world world) (target located-entity))
  (let ((result NIL)
        (loc (location target)))
    (do-container-tree (entity world result)
      (when (and (not (eq entity target))
                 (typep entity 'solid))
        (let ((hit (scan entity target)))
          (when (and hit
                     (or (null result)
                         (< (vsqrdist2 loc (hit-location hit))
                            (vsqrdist2 loc (hit-location result)))))
            (setf (hit-object hit) entity)
            (setf result hit)))))))

(defclass empty-world (world)
  ((packet :initform NIL)))

(defmethod initialize-instance :after ((world empty-world) &key)
  (enter (make-instance 'wall :location (vec 0 128)) world)
  (enter (make-instance 'wall :location (vec -512 0) :size (vec 32 1024)) world)
  (enter (make-instance 'wall :location (vec +512 0) :size (vec 32 1024)) world)
  (enter (make-instance 'wall :location (vec 0 -512) :size (vec 1024 32)) world)
  (enter (make-instance 'wall :location (vec 0 +512) :size (vec 1024 32)) world)
  ;;  (enter (make-instance 'guard :location (vec 0 256) :route '((128 254 0) (0 512 1) (-128 256 0))) world)
  (enter (make-instance 'guard :location (vec 0 -256)) world)
  (enter (make-instance 'player) world)
  (change-class world 'world))

(defmethod load-world ((world world))
  (load-world (packet world)))

(defmethod load-world ((pathname pathname))
  (with-packet (packet pathname :direction :input)
    (load-world packet)))

(defmethod load-world ((packet packet))
  (v:info :ld45.world "Loading ~a" packet)
  (destructuring-bind (header &optional metadata) (parse-sexps (packet-entry "meta.lisp" packet :element-type 'character))
    (decode-payload
     metadata (type-prototype 'world) packet
     (destructuring-bind (&key identifier version) header
       (assert (eql 'world identifier))
       (coerce-version version)))))

(defmethod save-world ((world world) (target (eql T)) &key version (if-exists :supersede))
  (let ((packet (packet world)))
    (with-packet (packet (storage packet) :offset (offset packet) :direction :output :if-exists if-exists)
      (save-world world packet :version version))))

(defmethod save-world (world (pathname pathname) &key version (if-exists :supersede))
  (with-packet (packet pathname :direction :output :if-exists if-exists)
    (save-world world packet :version version)))

(defmethod save-world ((world world) (packet packet) &key version)
  (v:info :ld45.world "Saving ~a to ~a" world packet)
  (let ((version (ensure-version version)))
    (with-packet-entry (stream "meta.lisp" packet :element-type 'character)
      (princ* (list :identifier 'world :version (type-of version)) stream)
      (princ* (encode-payload world NIL packet version) stream))))
