(in-package #:org.shirakumo.fraf.ld45)

(defgeneric scan (tested tester))
(defgeneric collide (object collided hit))

(defstruct (hit (:constructor make-hit (object time location normal)))
  (object NIL)
  (time 0.0 :type single-float)
  (location NIL :type vec2)
  (normal NIL :type vec2))

(defun aabb (seg-pos seg-vel aabb-pos aabb-size)
  (declare (type vec2 seg-pos seg-vel aabb-pos aabb-size))
  (sb-int:with-float-traps-masked (:overflow :underflow :inexact :invalid)
    (let* ((scale (vec2 (if (= 0 (vx seg-vel)) float-features:single-float-positive-infinity (/ (vx seg-vel)))
                        (if (= 0 (vy seg-vel)) float-features:single-float-positive-infinity (/ (vy seg-vel)))))
           (sign (vec2 (if (<= 0. (vx seg-vel)) +1. -1.)
                       (if (<= 0. (vy seg-vel)) +1. -1.)))
           (near (v* (v- (v- aabb-pos (v* sign aabb-size)) seg-pos) scale))
           (far  (v* (v- (v+ aabb-pos (v* sign aabb-size)) seg-pos) scale)))
      (unless (or (< (vy far) (vx near))
                  (< (vx far) (vy near)))
        (let ((t-near (max (vx near) (vy near)))
              (t-far (min (vx far) (vy far))))
          (when (and (< t-near 1)
                     (< 0 t-far))
            (let* ((time (clamp 0.0 t-near 1.0))
                   (normal (if (< (vy near) (vx near))
                               (vec (- (vx sign)) 0)
                               (vec 0 (- (vy sign))))))
              (unless (= 0 (v. normal seg-vel))
                ;; KLUDGE: This test is necessary in order to ignore vertical edges
                ;;         that seem to stick out of the blocks. I have no idea why.
                (unless (and (/= 0 (vy normal))
                             (<= (vx aabb-size) (abs (- (vx aabb-pos) (vx seg-pos)))))
                  (make-hit NIL time aabb-pos normal))))))))))

(defun rayline (ray dir a b)
  (declare (type vec2 ray dir a b))
  (let* ((lin (v- b a))
         (div (+ (* (- (vx2 lin)) (vy2 dir))
                 (* (+ (vx2 dir)) (vy2 lin)))))
    (when (/= 0.0 div)
      (let ((r (/ (+ (* (- (vy2 dir)) (- (vx2 ray) (vx2 a)))
                     (* (+ (vx2 dir)) (- (vy2 ray) (vy2 a))))
                  div))
            (l (/ (- (* (vx2 lin) (- (vy2 ray) (vy2 a)))
                     (* (vy2 lin) (- (vx2 ray) (vx2 a))))
                  div)))
        (when (and (<= 0 r 1) (<= 0 l 1))
          l)))))