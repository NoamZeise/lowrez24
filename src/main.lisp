#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage lowres24
	    (:use :cl)
	    (:export #:run))

(in-package :lowres24)

(defun run ()
  (gficl:with-window
   (:title "lowres24" :width 500 :height 500 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))
    (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.3) *cam-up*))
    (gficl:bind-gl *main-shader*)
    (gficl:bind-matrix
     *main-shader* "view"
     (gficl:view-matrix *cam-pos* (gficl:-vec *cam-pos*) *cam-up*))))

(defun draw ()
  (gficl:with-render
   (gficl:bind-gl *offscreen-fb*)
   (gl:viewport 0 0 *target-width* *target-height*)
   (gl:clear-color 0 1 0 0)
   (gl:clear :color-buffer :depth-buffer)
   (gl:enable :depth-test)
   (gficl:bind-gl *main-shader*)
   (gficl:draw-vertex-data *cube*)
   (if *offscreen-resolve-fb*
      (gficl:blit-framebuffers *offscreen-fb* *offscreen-resolve-fb*
			       *target-width* *target-height*))
   (final-pass)))

(defun final-pass ()
  (gl:bind-framebuffer :framebuffer 0)
  (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (gl:disable :depth-test)
  (gficl:bind-gl *post-shader*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d
		   (gficl:framebuffer-texture-id
		    (if *offscreen-resolve-fb* *offscreen-resolve-fb* *offscreen-fb*)
		    0))
  (gficl:bind-gl *dummy-data*)
  (gl:draw-arrays :triangles 0 3))

(defun resize (w h)
  (gficl:bind-gl *post-shader*)
  (gficl:bind-matrix *post-shader* "transform"
		     (gficl:target-resolution-matrix *target-width* *target-height* w h)))

(defun setup ()
  (setf *offscreen-resolve-fb* nil)
  (let ((samples 0))
    (if (> (setf samples (min 32 (gl:get-integer :max-samples))) 1)
	(setf *offscreen-resolve-fb*
	      (gficl:make-framebuffer
	       (list (gficl:make-attachment-description :color-attachment0 :texture))
	       *target-width* *target-height*)))
    (setf *offscreen-fb*
	  (gficl:make-framebuffer
	   (list (gficl:make-attachment-description
		  :color-attachment0 (if *offscreen-resolve-fb* :renderbuffer :texture))
		 (gficl:make-attachment-description :depth-stencil-attachment))
	   *target-width* *target-height* samples)))
  (setf *main-shader* (gficl:make-shader *main-vert* *main-frag*))
  (setf *post-shader* (gficl:make-shader *post-vert* *post-frag*))
  (gficl:bind-gl *post-shader*)
  (gl:uniformi (gficl:shader-loc *post-shader* "screen") 0)
  (setf *dummy-data* (gficl:make-vertex-data
		      (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
		      '(((0))) '(0 0 0)))
  (setf *cube* (gficl:make-vertex-data
		*vert-form* (getf *cube-data* :verts) (getf *cube-data* :indices)))
  (setf *cam-pos* (gficl:make-vec '(5 2 3)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "model" (gficl:make-matrix))
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  ;(gl:enable :cull-face)
  (resize (gficl:window-width) (gficl:window-height)))

(defun cleanup ()
  (if *offscreen-resolve-fb* (gficl:delete-gl *offscreen-resolve-fb*))
  (gficl:delete-gl *offscreen-fb*)
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *cube*)
  (gficl:delete-gl *post-shader*)
  (gficl:delete-gl *dummy-data*))


;;; ---- Globals ----

(defparameter *cam-up* (gficl:make-vec '(0 0 1)))
(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)

(defparameter *target-width* 64)
(defparameter *target-height* 64)
(defparameter *offscreen-fb* nil)
(defparameter *offscreen-resolve-fb* nil)

(defparameter *vert-form*
	      (gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :float))))
(defparameter *cube-data*
	      (list :verts
		    '(((-1 -1 -1))
		      ((-1 -1  1))
		      ((-1  1 -1))
		      ((-1  1  1))
		      (( 1 -1 -1))
		      (( 1 -1  1))
		      (( 1  1 -1))
		      (( 1  1  1)))
		    :indices
		    '(2 1 0 1 2 3
			4 5 6 7 6 5
			0 1 4 5 4 1
			6 3 2 3 6 7
			4 2 0 2 4 6
			1 3 5 7 5 3)))

(defparameter *cube* nil)

(defparameter *dummy-data* nil)

;;; ---- Shaders ----

(defparameter *main-vert*
	      "#version 330
layout (location = 0) in vec3 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 pos;

void main() {
  pos = position;
  gl_Position = projection * view * model * vec4(pos, 1);
}")

(defparameter *main-frag*
  "#version 330
in vec3 pos;
out vec4 colour;

void main() {
   vec3 p = (pos + vec3(3)) * 0.2;
   colour = vec4(p.x, p.y, p.z, 1);
}")

(defparameter *main-shader* nil)

(defparameter *post-vert*
	      "#version 330
out vec2 uv;
uniform mat4 transform;
void main() {
  uv = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
  gl_Position = transform * vec4(uv * 2.0f - 1.0f, 0.0f, 1.0f);
}")

(defparameter *post-frag*
  "#version 330
in vec2 uv;
out vec4 colour;
uniform sampler2D screen;
void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  colour = texture(screen, uv);
}")

(defparameter *post-shader* nil)
