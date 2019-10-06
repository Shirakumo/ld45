(in-package #:org.shirakumo.fraf.ld45)

(define-asset (ld45 game-over) image
    #p"game-over.png"
  :min-filter :nearest
  :mag-filter :nearest)

(define-shader-entity screen ()
  ((vertex-array :initform (asset 'trial 'trial::fullscreen-square) :accessor vertex-array)
   (texture :initarg :texture :accessor texture)))

(defmethod paint ((screen screen) (pass shader-pass))
  (let ((program (shader-program-for-pass pass screen))
        (vao (vertex-array screen)))
    (gl:bind-texture :texture-2d (gl-name (texture screen)))
    (with-pushed-attribs
      (disable :depth-test)
      (gl:bind-vertex-array (gl-name vao))
      (%gl:draw-elements :triangles (size vao) :unsigned-int (cffi:null-pointer))
      (gl:bind-vertex-array 0))))

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
in vec2 tex_coord;
out vec4 color;

void main(){
  color = texture(screen, tex_coord);
}")
