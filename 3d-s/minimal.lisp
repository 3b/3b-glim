#++
(ql:quickload '(alexandria 3b-glim/example/s))
(defpackage #:minimal-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex #:textured #:solid
           #:position #:uv
           #:mv #:mvp #:lut #:tex #:line-base #:line-step #:debug1))
(defpackage #:minimal
  (:use #:cl #:minimal-shaders #:3b-glim-example/s)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:glim #:3b-glim/s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shaders
(in-package #:minimal-shaders)
(input position :vec4 :location 0)
(input uv :vec4 :location 1)
(input color :vec4 :location 2)

(output color :vec4 :stage :fragment)

;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform tex :sampler-2d)
(uniform debug1 :int)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (uv :vec4)
  (color :vec4))

;; generic vertex shader used for a few lighting models
(defun vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs position) (* mv position)
        (@ outs uv) uv
        (@ outs color) color))

(defun textured ()
  (let* ((uv (@ ins uv)))
    (setf color
          (vec4 (vec3
                 (* 0.5 (1+ (.x (texture tex (.xy uv))))))
                1))))

(defun solid ()
  (setf color (@ ins color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code
(in-package #:minimal)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec4) ;; uv
     (2 :vec4) ;; color
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun color (r &optional (g 0.0) (b 0.0) (a 1.0))
  (glim:attrib-f 2 r g b a))

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))

(defclass minimal (scratchpad)
  ((tex :initform nil :accessor tex))
  (:default-initargs :shaders '((:tex :vertex vertex :fragment textured)
                                (:solid :vertex vertex :fragment solid))))


(defvar *w* nil)

(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview))))



(defun load-texture (w)
  (unless (tex w)
    (setf (tex w) (gl:gen-texture)))

  (gl:pixel-store :unpack-alignment 1)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d (tex w))

  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (let* ((w 512)
         (h 512)
         (a (make-array (list h w) :element-type 'single-float
                                   :initial-element 1.0)))
    (time
     (loop for j below h
           do (loop for i below w
                    do (setf (aref a j i)
                             (random 1.0)))))
    (gl:tex-image-2d :texture-2d 0 :r32f w h 0
                     :red :float (make-array (* w h)
                                             :element-type 'single-float
                                             :displaced-to a))))



(defmethod display ((w minimal) now)
  (setf *w* w)

  (glim:with-state (*format*)
    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :multisample :texture-2d)

    (when (tex w)
      (gl:active-texture 0)
      (gl:enable :texture-2d)

      (gl:bind-texture :texture-2d (tex w))

      (glim:uniform 'tex 0)

      (glim:uniform 'debug1 (if (aref *flags* 1) 1 0))

      (glim:with-pushed-matrix (:modelview)
        (glim:scale 1 1 1)
        (glim:scale 1.8 1.8 1)
        (glim:translate -0.5 -0.5 0)
        (gl:line-width 10)
        (uniforms)

        (glim:with-draw (:quads :shader :tex)

          (uv 0 1 0 0)
          (vertex 0 0)

          (uv 1 1 0 0)
          (vertex 1 0)

          (uv 1 0 0 0)
          (vertex 1 1)

          (uv 0 0 0 0)
          (vertex 0 1))
        (dispatch-draws w)))

    (gl:line-width 1)
    (let ((*random-state* (make-random-state *random-state*)))
      (glim:with-draw (:lines :shader :solid)
        (loop for i below 10000
              when (evenp i)
                do (color (random 1.0) (random 1.0) (random 1.0))
              do (vertex (* 12 (1- (random 2.0)))
                         (* 12 (1- (random 2.0)))
                         0
                         1))))
    (dispatch-draws w)))

(defmethod mouse ((window minimal) button state x y)
  (format t "click ~s ~s~%" button state))

(defmethod keyboard ((window minimal) key x y)
  (declare (ignore x y))

  (case key
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\space
     (load-texture window))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod init-gl ((w minimal))
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun minimal (&rest args)
  (glut:display-window (apply #'make-instance 'minimal args)))

#++
(ql:quickload 'minimal)
#++
(minimal :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)

