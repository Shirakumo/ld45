(in-package #:org.shirakumo.fraf.ld45)

(defstruct (endpoint (:include vec2)
                     (:constructor make-endpoint (3d-vectors::%vx2
                                                  3d-vectors::%vy2))
                     (:copier NIL)
                     (:predicate NIL))
  (segment NIL)
  (begin NIL :type boolean)
  (angle 0f0 :type single-float))

(defun endpoint< (a b)
  (cond ((< (endpoint-angle a) (endpoint-angle b)) T)
        ((< (endpoint-angle b) (endpoint-angle a)) NIL)
        (T (endpoint-begin a))))

(defun segment-in-front-p (a b p)
  (flet ((left-p (p a b)
           (< (- (* (- (vx2 p) (vx2 a)) (- (vy2 b) (vy2 a)))
                 (* (- (vy2 p) (vy2 a)) (- (vx2 b) (vx2 a))))
              0)))
    (let ((a1 (left-p (vlerp (car b) (cdr b) 0.01) (car a) (cdr a)))
          (a2 (left-p (vlerp (cdr b) (car b) 0.01) (car a) (cdr a)))
          (a3 (left-p p (car a) (cdr a)))
          (b1 (left-p (vlerp (car a) (cdr a) 0.01) (car b) (cdr b)))
          (b2 (left-p (vlerp (cdr a) (car a) 0.01) (car b) (cdr b)))
          (b3 (left-p p (car b) (cdr b))))
      (or (and (eq b1 b2) (not (eq b2 b3)))
          (and (eq a1 a2) (eq a2 a3))))))

(define-shader-entity viewcone (vertex-entity located-entity)
  ((direction :initform (vec 0 0) :accessor direction)
   (aperture :initarg :aperture :initform (->rad 16) :accessor aperture)
   (radius :initarg :radius :initform 512 :accessor radius)
   (segments :initform (make-array 0 :adjustable T :fill-pointer T) :accessor segments)
   (endpoints :initform (make-array 0 :adjustable T :fill-pointer T) :accessor endpoints)))

(defmethod initialize-instance :after ((viewcone viewcone) &key)
  (let ((vbo (make-instance 'vertex-buffer :buffer-data (make-array 0 :adjustable T :fill-pointer T :element-type 'single-float))))
    (setf (vertex-array viewcone) (make-instance 'vertex-array
                                                 :bindings `((,vbo :size 2 :offset 0 :stride 8))
                                                 :vertex-form :triangle-fan
                                                 :size 0))))

(defmethod (setf location) :after (p (viewcone viewcone))
  (loop for (a . b) across (segments viewcone)
        do (setf (endpoint-angle a) (normalize-angle (atan (- (vy a) (vy p)) (- (vx a) (vx p)))))
           (setf (endpoint-angle b) (normalize-angle (atan (- (vy b) (vy p)) (- (vx b) (vx p)))))
           (let ((dangle (- (endpoint-angle b) (endpoint-angle a))))
             (when (<= dangle (- PI)) (incf dangle (* 2 PI)))
             (when (< PI dangle) (decf dangle (* 2 PI)))
             (setf (endpoint-begin a) (< 0 dangle))
             (setf (endpoint-begin b) (not (endpoint-begin a)))))
  (sort (endpoints viewcone) #'endpoint<)
  (compute-viewcone viewcone))

(defmethod (setf direction) :after (_ (viewcone viewcone))
  (compute-viewcone viewcone))

(defmethod (setf aperture) :after (_ (viewcone viewcone))
  (compute-viewcone viewcone))

(defmethod (setf radius) :after (_ (viewcone viewcone))
  (compute-viewcone viewcone))

(defmethod gather-segments (thing segments points))

(defmethod gather-segments ((world world) segments points)
  (for:for ((entity over world))
    (gather-segments entity segments points)))

(defmethod gather-segments ((wall wall) segments points)
  (let* ((loc (location wall))
         (bsize (bsize wall))
         (l (- (vx loc) (vx bsize)))
         (r (+ (vx loc) (vx bsize)))
         (b (- (vy loc) (vy bsize)))
         (u (+ (vy loc) (vy bsize))))
    (flet ((add (ax ay bx by)
             (let* ((a (make-endpoint ax ay))
                    (b (make-endpoint bx by))
                    (s (cons a b)))
               (setf (endpoint-segment a) s)
               (setf (endpoint-segment b) s)
               (vector-push-extend s segments)
               (vector-push-extend a points)
               (vector-push-extend b points))))
      (add l b  r b)
      (add r b  r u)
      (add r u  l u)
      (add l u  l b))))

(defmethod update-scene-cache ((viewcone viewcone) (scene scene))
  (let ((endpoints (endpoints viewcone))
        (segments (segments viewcone)))
    (setf (fill-pointer endpoints) 0)
    (setf (fill-pointer segments) 0)
    (gather-segments scene segments endpoints)))

(defun line-intersection (p1 p2 p3 p4)
  (let ((s (/ (- (* (- (vx p4) (vx p3)) (- (vy p1) (vy p3)))
                 (* (- (vy p4) (vy p3)) (- (vx p1) (vx p3))))
              (- (* (- (vy p4) (vy p3)) (- (vx p2) (vx p1)))
                 (* (- (vx p4) (vx p3)) (- (vy p2) (vy p1)))))))
    (vec (+ (vx p1) (* s (- (vx p2) (vx p1))))
         (+ (vy p1) (* s (- (vy p2) (vy p1)))))))

(defun rayline* (ray dir a b)
  (declare (type vec2 ray dir a b))
  (let* ((lin (v- b a))
         (div (+ (* (- (vx2 lin)) (vy2 dir))
                 (* (+ (vx2 dir)) (vy2 lin)))))
    (when (/= 0.0 div)
      (/ (- (* (vx2 lin) (- (vy2 ray) (vy2 a)))
            (* (vy2 lin) (- (vx2 ray) (vx2 a))))
         div))))

(defmethod compute-viewcone ((viewcone viewcone))
  (let* ((loc (location viewcone))
         (r (radius viewcone))
         (open (list NIL))
         (begin-angle 0f0)
         (vao (vertex-array viewcone))
         (vbo (caar (bindings vao)))
         (data (buffer-data vbo)))
    (setf (fill-pointer data) 0)
    (labels ((add-vert (p rf)
               (vector-push-extend (* (cos p) r rf) data)
               (vector-push-extend (* (sin p) r rf) data))
             (add (a1 a2 seg)
               (let ((dir1 (v* (vec (cos a1) (sin a1)) r))
                     (dir2 (v* (vec (cos a2) (sin a2)) r))
                     p3 p4)
                 (cond (seg
                        (setf p3 (car seg))
                        (setf p4 (cdr seg)))
                       (T
                        (setf p3 (vec (+ (vx loc) (* (cos a1) r))
                                      (+ (vy loc) (* (sin a1) r))))
                        (setf p4 (vec (+ (vx loc) (* (cos a2) r))
                                      (+ (vy loc) (* (sin a2) r))))))
                 (let ((r1 (min (rayline* loc dir1 p3 p4) 1))
                       (r2 (min (rayline* loc dir2 p3 p4) 1)))
                   (add-vert a1 r1)
                   (when (or (= 1 r1) (= 1 r2))
                     ;; FIXME: This is really inefficient for obvious reasons.
                     (loop for i = a1 then (mod (+ i (/ PIF 32)) (* 2 PIF))
                           for dir = (v* (vec (cos i) (sin i)) r)
                           until (< (abs (- i a2)) (/ PIF 32))
                           do (add-vert i (min (rayline* loc dir p3 p4) 1))))
                   (add-vert a2 r2)))))
      (vector-push-extend 0f0 data)
      (vector-push-extend 0f0 data)
      (dotimes (pass 2)
        (loop for p across (endpoints viewcone)
              for s = (endpoint-segment p)
              for current-old = (second open)
              do (cond ((endpoint-begin p)
                        (loop for cons on open
                              until (cond ((null (cdr cons))
                                           (setf (cdr cons) (list s)))
                                          ((not (segment-in-front-p s (second cons) loc))
                                           (setf (cdr cons) (list* s (cdr cons)))))))
                       (T
                        (setf open (delete s open))))
                 (let ((current-new (second open)))
                   (unless (eq current-old current-new)
                     (when (= 1 pass)
                       (add begin-angle (endpoint-angle p) current-old))
                     (setf begin-angle (endpoint-angle p)))))))
    (resize-buffer vbo (* (length data) 4) :data data)
    (setf (size vao) (length data))))

(define-class-shader (viewcone :fragment-shader)
  "out vec4 color;
void main(){
  color = vec4(1, 0, 0, 0.3);
}")
