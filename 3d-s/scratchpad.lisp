(defpackage #:3b-glim-example/s
  (:use :cl #:3b-glim-example/s-shaders)
  (:local-nicknames (#:glim #:3b-glim/s))
  (:export #:run #:dispatch-draws #:scratchpad
           #:wx
           #:wy
           #:display
           #:keyboard
           #:mx
           #:my
           #:init-gl
           #:entry
           #:mouse
           #:mouse-wheel))
(in-package 3b-glim-example/s)

(defclass scratchpad (glut:window)
  ((shaders :accessor shaders :initform nil :initarg :shaders)
   (start :initform nil :accessor start)
   (mx :initform 0 :accessor mx)
   (my :initform 0 :accessor my)
   (wx :initform 0 :accessor wx)
   (wy :initform 0 :accessor wy)
   (programs :reader programs :initform (make-hash-table)))
  (:default-initargs :width 1024 :height 1024 :title "3b-glim/s scratchpad"
                     :mode '(:double :rgb :depth :multisample)
                     :right-menu '(:click :exit)))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))

(defmethod reload-shader ((w scratchpad) key stage entry &rest more)
  (let ((stages (list* stage entry more))
        (old (gethash key (programs w))))
    (if (assoc key (shaders w))
        (setf (cdr (assoc key (shaders w))) stages)
        (push (list* key stages) (shaders w)))
    (with-simple-restart (continue "skip compiling ~s" key)
      (setf (gethash key (programs w))
            (multiple-value-list
             (3bgl-shaders::reload-program* (first old) stages :print t))))))

(defmethod init-gl ((w scratchpad)))

(defmethod glut:display-window :before ((w scratchpad))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (setf (start w) (coerce (/ (get-internal-real-time)
                             internal-time-units-per-second)
                          'double-float))
  (loop for s in (shaders w)
        do (apply #'reload-shader w s))
  (init-gl w))


(defparameter *modified-shader-functions* nil)

(defun modified-shader-hook (modified)
  (format t "saw modified functions ~s~%" modified)
  (setf *modified-shader-functions*
        (union modified *modified-shader-functions*)))
(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)
(setf 3bgl-shaders::*print-shaders* t)

(defun recompile-modified-shaders (w)
  (let* ((m *modified-shader-functions*)
         (recompile nil))
    ;; flag any shaders we are using
    (setf recompile
          (loop for shader in (shaders w)
                for ep = (loop for (nil a) on (cdr shader) by #'cddr collect a)
                when (intersection ep m)
                  collect shader))
    ;; fixme: this needs a lock, since it could be modified from
    ;; another thread
    (setf *modified-shader-functions* nil)
    (when recompile
      (format t "~%recompiling shader program for changes in functions:~&  ~a~% = ~s~%"
              m recompile)
      (loop for s in recompile
            do (with-simple-restart (continue "skip compiling ~s" s)
                 (apply #'reload-shader w s))))))


(defvar *r* 0)
(defvar *frames* 0)
(defvar *frametime* nil)



(defun fps ()
  (unless *frametime*
    (setf *frametime* (get-internal-real-time))
    (setf *frames* 1))
  (incf *frames*)
  (let* ((n (get-internal-real-time))
         (s (float (/ (- n *frametime*)
                      internal-time-units-per-second))))
    (when (>= s 10)
      (format t "~&~s frames in ~s sec = ~s fps (~s s)~%"
              *frames* s (/ *frames* s) (/ s *frames*))
      (setf *frametime* n)
      (setf *frames* 0))))


(defmethod display ((window scratchpad) now))

(defmethod dispatch-draws ((w scratchpad))
  (let* ((buffers (glim:get-buffers))
         (draws (glim:get-draws))
         (vbos (gl:gen-buffers (length (getf buffers :vbo-sizes))))
         (ibo (gl:gen-buffer))
         (enabled-atts (make-hash-table)))
    (unwind-protect
         (progn
           ;; upload the index data (we allocate with no data, then
           ;; buffer-sub-data the individual pieces)
           (gl:bind-buffer :element-array-buffer ibo)
           (%gl:buffer-data :element-array-buffer (getf buffers :ibo-size)
                            (cffi:null-pointer) :stream-draw)
           (loop for offset = 0 then (+ offset s)
                 for (s b) in (getf buffers :ibo-chunks)
                 do (cffi:with-pointer-to-vector-data (p b)
                      (%gl:buffer-sub-data :element-array-buffer
                                           offset s
                                           p)))

           ;; upload vbo data
           (loop for vbo-size across (getf buffers :vbo-sizes)
                 for vbo-chunks across (getf buffers :vbo-chunks)
                 for vbo in vbos
                 do (gl:bind-buffer :array-buffer vbo)
                    (%gl:buffer-data :array-buffer vbo-size
                                     (cffi:null-pointer) :stream-draw)
                    (loop for offset = 0 then (+ offset s)
                          for (s b) in vbo-chunks
                          do (cffi:with-pointer-to-vector-data (p b)
                               (%gl:buffer-sub-data :array-buffer
                                                    offset s p))))


           ;; set up vertex attributes
           (loop for buffer in (glim::buffers
                                (glim::vertex-format glim::*state*))
                 for stride = (glim::stride buffer)
                 for vbo in vbos
                 do (gl:bind-buffer :array-buffer vbo)
                    (loop for att in (glim::attributes buffer)
                          for offset = (glim::offset att)
                          for (nil count nil type)
                            = (gethash (glim::element-type att)
                                       glim::*attribute-types*)
                          for loc = (glim::attribute-index att)
                          do (gl:enable-vertex-attrib-array loc)
                             (setf (gethash loc enabled-atts) loc)
                             (%gl:vertex-attrib-pointer
                              loc count type t stride offset)))
           ;; dispatch draws
                                        ;(break "draws ~s" draws buffers glim::*state*)
           (loop for draw in draws
                 for uniforms = (glim::uniforms draw)
                 for index-base = (glim::index-base draw)
                 for index-count = (glim::index-count draw)
                 for primitive = (glim::primitive draw)
                 for shader-id = (glim::shader draw)
                 for vertex-base = (glim::vertex-base draw)
                 for vertex-count = (glim::vertex-count draw)
                 for (program uniformh) = (gethash shader-id (programs w))
                 when program
                   do (gl:use-program program)
                      (when uniformh
                        (loop for u being the hash-keys of uniforms
                                using (hash-value v)
                              for uu = (gethash u uniformh)
                              for (ui nil nil ut) = uu
                              do (when (and ui (not (minusp ui)))
                                   (ecase ut
                                     ((:int
                                       :sampler-1d
                                       :sampler-2d
                                       :sampler-3d)
                                      (gl:uniformi ui v))
                                     ((:bool)
                                      (gl:uniformi ui (if v 1 0)))
                                     ((:ivec2 :ivec3 :ivec4)
                                      (if (numberp v)
                                          (gl:uniformi ui v 0 0 0)
                                          (gl:uniformiv ui v)))
                                     (:float (gl:uniformf ui v))
                                     ((:vec2 :vec3 :vec4)
                                      (if (numberp v)
                                          (gl:uniformf ui v 0 0 0)
                                          (gl:uniformfv ui v)))
                                     (:mat4 (gl:uniform-matrix-4fv ui v nil))))))
                      (if index-base
                          (when (plusp index-count)
                            (assert (plusp index-count))
                            (%gl:draw-elements-base-vertex
                             primitive index-count
                             :unsigned-short (* index-base 2)
                             vertex-base))
                          (%gl:draw-arrays primitive vertex-base vertex-count))))
      ;; clean up
      (gl:use-program 0)
      (map nil 'gl:disable-vertex-attrib-array
           (alexandria:hash-table-keys enabled-atts))
      (gl:delete-buffers (list* ibo vbos)))))

(defmethod glut:display ((window scratchpad))
  (with-simple-restart (continue "continue")
    (fps)
    (recompile-modified-shaders window)
    (let ((now (coerce (- (coerce (/ (get-internal-real-time)
                                     internal-time-units-per-second)
                                  'double-float)
                          (start window))
                       'single-float)))
      (flet ((s (x)
               (* 0.1 (abs (sin (/ now x))))))
        (gl:clear-color (s 2) (s 3) (s 4) 1)
        (gl:clear :color-buffer :depth-buffer)
        (display window now)
        (glut:swap-buffers)))))

(defmethod entry (w state))
(defmethod glut:entry ((window scratchpad) state)
  (entry window state))
(defmethod glut:reshape ((window scratchpad) width height)
  (setf (wx window) width (wy window) height)
  (gl:viewport 0 0 width height)
  (glim:matrix-mode :projection)
  (glim:load-identity)
  (glim:perspective 50 (if (zerop height) 1 (/ width height)) 0.5 20)
  (glim:matrix-mode :modelview)
  (glim:load-identity))


(defmethod glut::menu ((window scratchpad) menu id)
  (format t "~&got menu item ~s from menu ~s~%" menu id)

  (case id
    (:click
     (setf (glut::right-menu window)
           `(:click :exit)))
    (:exit
     (glut:destroy-current-window))))

(defmethod keyboard ((window scratchpad) key x y)
  )

(defmethod glut:keyboard ((window scratchpad) key x y)
  (case key
    (#\Esc
     (glut:destroy-current-window))
    (otherwise (keyboard window key x y))))

(defmethod glut:special ((window scratchpad) key x y)
  (keyboard window key x y))

(defmethod mouse ((window scratchpad) button state x y)
  )

(defmethod glut:mouse ((window scratchpad) button state x y)
  (mouse window button state x y))

(defmethod mouse-wheel ((window scratchpad) button state x y)
  )

(defmethod glut:mouse-wheel ((window scratchpad) button state x y)
  (mouse-wheel window button state x y))

(defmethod glut:motion ((window scratchpad) x y)
  (setf (mx window) x
        (my window) y))

(defmethod glut:passive-motion ((window scratchpad) x y)
  (setf (mx window) x
        (my window) y))


(defvar *w* nil)
(defmethod glut:idle ((window scratchpad))
  (unless *w*
    (setf *w* window)
    (format t "foo!~%"))
  (glut:post-redisplay))

(defun run (class &rest args)
  (setf *w* nil)
  (glut:display-window (apply #'make-instance class args)))
