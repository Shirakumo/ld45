(in-package #:org.shirakumo.fraf.ld45)

(define-pool ld45
  :base :ld45)

(defmacro define-global (name value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if (boundp ',name)
         (setf ,name ,value)
         #+sbcl (sb-ext:defglobal ,name ,value)
         #-sbcl (defvar ,name ,value))))

(defun clamp (low mid high)
  (max low (min mid high)))

(defun ->rad (deg)
  (* PI (/ deg 180)))

(defun ->deg (rad)
  (/ (* rad 180) PI))

(defun vrand (min max)
  (vec (+ min (random (- max min)))
       (+ min (random (- max min)))))

(defun vrandr (min max)
  (let ((r (+ min (random (- max min))))
        (phi (random (* 2 PI))))
    (vec (* r (cos phi))
         (* r (sin phi)))))

(defun vsqrdist2 (a b)
  (declare (type vec2 a b))
  (declare (optimize speed))
  (+ (expt (- (vx2 a) (vx2 b)) 2)
     (expt (- (vy2 a) (vy2 b)) 2)))

(defun nvalign (vec grid)
  (vsetf vec
         (* grid (floor (+ (vx vec) (/ grid 2)) grid))
         (* grid (floor (+ (vy vec) (/ grid 2)) grid))))

(defun point-angle (point)
  (atan (vy point) (vx point)))

(defun type-prototype (type)
  (case type
    (character #\Nul)
    (complex #c(0 0))
    (cons '(NIL . NIL))
    (float 0.0)
    (function #'identity)
    (hash-table (load-time-value (make-hash-table)))
    (integer 0)
    (null NIL)
    (package #.*package*)
    (pathname #p"")
    (random-state (load-time-value (make-random-state)))
    (readtable (load-time-value (copy-readtable)))
    (stream (load-time-value (make-broadcast-stream)))
    (string "string")
    (symbol '#:symbol)
    (vector #(vector))
    (T (let ((class (find-class type)))
         (unless (c2mop:class-finalized-p class)
           (c2mop:finalize-inheritance class))
         (c2mop:class-prototype class)))))

(defun query (message &key default parse)
  (flet ((parse (string)
           (cond ((not string))
                 ((string= "" string)
                  default)
                 (T
                  (if parse (funcall parse string) string)))))
    (let ((message (format NIL "~a~@[ [~a]~]:" message default)))
      (format *query-io* "~&~a~%> " message)
      (parse (read-line *query-io* NIL)))))


(defmacro with-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*package* #.*package*)
           (*print-case* :downcase)
           (*print-readably* NIL))
       ,@body)))

(defun parse-sexps (string)
  (with-io-syntax
    (loop with eof = (make-symbol "EOF")
          with i = 0
          collect (multiple-value-bind (data next) (read-from-string string NIL EOF :start i)
                    (setf i next)
                    (if (eql data EOF)
                        (loop-finish)
                        data)))))

(defun princ* (expression &optional (stream *standard-output*))
  (with-io-syntax
    (write expression :stream stream :case :downcase)
    (fresh-line stream)))

(defclass game-entity (entity)
  ((state :initform NIL :accessor state)))

(defmethod die :after ((entity game-entity))
  (setf (state entity) :dead))

(defmethod die :around ((entity game-entity))
  (unless (eq :dead (state entity))
    (call-next-method)))

(defclass solid (game-entity) ())

(defclass located-entity (entity)
  ((location :initarg :location :initform (vec 0 0) :accessor location
             :type vec2 :documentation "The location in 2D space.")))

(defmethod print-object ((entity located-entity) stream)
  (print-unreadable-object (entity stream :type T :identity T)
    (format stream "~a" (location entity))))

(defmethod paint :around ((obj located-entity) target)
  (with-pushed-matrix ()
    (translate-by (round (vx (location obj))) (round (vy (location obj))) 0)
    (call-next-method)))
