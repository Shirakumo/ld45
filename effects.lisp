(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 game-over) image
    #p"game-over.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-shader-subject screen ()
  ((fade-timer :initform 1f0 :accessor fade-timer)
   (vertex-array :initform (asset 'trial 'trial::fullscreen-square) :accessor vertex-array)
   (texture :initarg :texture :accessor texture)))

(define-handler (screen tick) (ev dt)
  (setf (fade-timer screen) (max 0 (- (fade-timer screen) dt))))

(defmethod paint ((screen screen) (pass shader-pass))
  (let ((program (shader-program-for-pass pass screen))
        (vao (vertex-array screen)))
    (setf (uniform program "strength") (float (- 1 (fade-timer screen)) 0f0))
    (gl:bind-texture :texture-2d (gl-name (texture screen)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-elements :triangles (size vao) :unsigned-int (cffi:null-pointer))))

(define-class-shader (screen :vertex-shader)
  "layout(location = 0) in vec3 position;
layout(location = 1) in vec2 in_tex_coord;
out vec2 tex_coord;
void main(){  
  gl_Position = vec4(position, 1.0f);
  tex_coord = in_tex_coord;
}")

(define-class-shader (screen :fragment-shader)
  "uniform sampler2D screen;
uniform float strength = 0.0;
in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(screen, tex_coord)*strength;
}")

(define-shader-pass fog-of-war (per-object-pass)
  ((color :port-type output :attachment :color-attachment0 :texspec (:internal-format :red))))

(defmethod coerce-pass-shader ((pass fog-of-war) object (type (eql :fragment-shader)))
  (trial::effective-shader type pass))

(defmethod paint-with ((pass fog-of-war) (scene scene))
  (let ((player (unit :player scene)))
    (cond ((or (capable-of :clairvoyance player)
               (active-p (unit :editor scene)))
           (gl:clear-color 1 1 1 1)
           (gl:clear :color-buffer)
           (gl:clear-color 0 0 0 0))
          (T
           (paint (viewcone player) pass)))))

(define-class-shader (fog-of-war :fragment-shader)
  "out vec4 color;
void main(){
  color = vec4(1);
}")

(define-shader-pass lighting-pass (render-pass)
  ((fog :port-type input)))

(define-class-shader (lighting-pass :fragment-shader)
  "uniform sampler2D fog; 
out vec4 color;

void main(){
  color *= texelFetch(fog, ivec2(gl_FragCoord.xy), 0).r;
}")
