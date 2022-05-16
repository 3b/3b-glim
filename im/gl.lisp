(defpackage #:3b-glim/gl
  (:use :cl)
  (:export #:load-shaders #:draw-buffers
           #:with-state/gl
           #:init-state/gl
           #:ensure-matrix))
(in-package #:3b-glim/gl)

;;;; probably should have 2-3 paths that can be detected and switched at
;;;; runtime:
;; todo: figure out exact cutoffs for various features
;;; es2: direct vertex-attrib-pointer etc + draw-arrays/draw-elements
;;;    simple shaders, etc
;;
;;; midrange: (es2+exts, gl 3.?+?) VAOs, multidraw etc
;;
;;; high-end: (gl 4.3+, es 3.2?) VAOs, multi-draw-arrays-indirect,
;;;   tess shaders for points/lines, etc


;;;; or maybe separate splits on CPU and GPU
;;; host:
;;
;;; direct pointer specification per call (es2)
;;
;;; VAOs (gl3+/ARB_vertex_array_object, es3+/OES_vertex_array_object)
;;
;;; multi-draw (gl 4.3+/ARB_multi_draw_indirect, es w/EXT_multi_draw_indirect)
;;
;;; GPU:
;;
;;; no tess/ssbo (use extra verts+attribs + vertex shader for points/lines)
;;
;;; tess+ssbo (gl 4.0/ARB_tessellation_shader, es 3.2/EXT_tessellation_shader)
;; (ssbo is gl 4.3/ARB_shader_storage_buffer_object, or es 3.1. looks
;;  like most ES3.x devices with tess are 3.1+)
;;;  combining with ssbo misses some old hardware (hd 4000, gtx 580,
;;;  etc) that have tess, but probably not enough to care
;;;  about. available in hd 4400.
;; requiring SSBO misses all mac, so maybe add ubo option later?

;;; possibly require glsl 1.30? in/out, switch/case/default, etc
;; essl 100 uses attribute/varying, so probably might as well support
;; older glsl too
;;; or 330 for explicit attrib locations?


(defclass glim-gl-state (3b-glim::renderer-config)
  ((shader :accessor shader :initarg :shader)
   (uniforms :accessor uniforms :initform (make-hash-table))
   (light-uniforms :accessor light-uniforms :initform ())
   (batching :reader batching :initform :simple :initarg :batching)
   (ssbo :reader ssbo :initform nil :initarg :ssbo)
   (api :reader api :initarg :api)
   (dims :accessor dims :initform (3b-glim::v4 0 0 1 1))))

(defun canonicalize-api (api)
  (assert (member api '(:opengl :gl :es :gles :es2)))
  (if (member api '(:opengl :gl))
      :gl
      :gles))

(defun configure-renderer* (api major minor extension-list)
  (declare (ignorable minor))
  (setf api (canonicalize-api api))
  ;; not supporting use with multiple renderer backends at once for now
  (when (and
         (3b-glim::renderer-config 3b-glim:*state*)
         (not (eql api (api (3b-glim::renderer-config 3b-glim:*state*)))))
    (cerror "Continue"
            "warning: changing renderer in already configured 3b-glim state"))
  (flet ((ext (x)
           (member x extension-list :test 'string=))
         (v (maj min)
           (or (> major maj)
               (and (= major maj)
                    (>= minor min)))))
    ;; check some basic features we require (some checks may be
    ;; redundant, but separate per feature to make it easier to
    ;; check/fix them)
    (if (eql api :gl)
        (assert (v 3 0)) ;; don't support old shader languages for now
        (assert (or (ext "GL_OES_standard_derivatives")
                    (v 3 0))))
    ;; decide if we will use tessellation shaders for points/lines
    (setf (3b-glim::%use-tess 3b-glim:*state*)
          (if (eql api :gl)
              ;; we require ssbo along with tess for gl/es backend to
              ;; reduce # of shader combinations
              (and (or (ext "GL_ARB_tessellation_shader")
                       ;; GL_EXT_tessellation_shader ?
                       (v 4 0))
                   (or (ext "GL_ARB_shader_storage_buffer_object")
                       (v 4 3)))
              (and (or (ext "GL_EXT_tessellation_shader")
                       (ext "GL_OES_tessellation_shader")
                       (v 3 2))
                   ;; es3.1+ for ssbo, no ssbo ext on es
                   (v 3 1))))
    (setf (3b-glim::%renderer-config 3b-glim:*state*)
          (make-instance
           'glim-gl-state
           :shader nil
           :api api
           :batching
           (if (eql api :gl)
               (cond
                 ;; assuming anything with multi_draw_indirect also has VAOs
                 ((or (ext "GL_ARB_multi_draw_indirect"
                           ;; "GL_AMD_multi_draw_indirect" ?
                           )
                      (v 4 3))
                  :multi-draw)
                 ((or (ext "GL_ARB_vertex_array_object")
                      (v 3 0))
                  :vao)
                 (t :simple))
               (cond
                 ((ext "GL_EXT_multi_draw_indirect")
                  :multi-draw)
                 ((or (ext "GL_OES_vertex_array_object")
                      (v 3 0))
                  :vao)
                 (t :simple)))))))

(defun configure-renderer (&optional (api :gl))
  ;; is there any way to detect API?
  (configure-renderer* api (gl:major-version) (gl:minor-version)
                       (gl:extension-list)))


(defparameter *modified-shader-functions* nil)
(defparameter *debug* t)

(defun modified-shader-hook (modified)
  (format t "saw modified functions ~s~%" modified)
  (setf *modified-shader-functions*
        (union modified *modified-shader-functions*)))

(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)

(setf 3bgl-shaders::*print-shaders* t)


(defun get-light-unforms (config)
  (setf (light-uniforms config) nil)
  (when (shader config)
    (flet ((u (i n)
             (gl:get-uniform-location
              (shader config)
              (format nil "lights[~a].~a" i n))))
      (setf (light-uniforms config)
            (coerce
             (loop for i below 3b-glim::+max-lights+
                   collect (u i "position")
                   collect (u i "ambient")
                   collect (u i "diffuse")
                   collect (u i "specular")
                   collect (u i "spotDir")
                   collect (u i "spotParams")
                   collect (u i "attenuation"))
             'vector)))))

(defun load-shaders ()
  "Loads shaders used by glim into current GL context. Must call
CONFIGURE-RENDERER or CONFIGURE-RENDERER* first."
  (let* ((config (3b-glim::renderer-config 3b-glim:*state*))
         (api (api config)))
    (assert (member api '(:gl :gles)))
    (time
     (setf (values (shader config)
                   (uniforms config))
           (3bgl-shaders::reload-program
            (shader config)
            '3b-glim/gl-shaders:vertex
            '3b-glim/gl-shaders:fragment
            :version 330)))
    (setf (light-uniforms config)
          (get-light-unforms config))))

(defun recompile-modified-shaders ()
  (let* ((m *modified-shader-functions*)
         (recompile nil))
    ;; flag any shaders we are using
    (setf recompile (intersection '(3b-glim/gl-shaders:fragment
                                    3b-glim/gl-shaders:vertex)
                                  m))
    ;; fixme: this needs a lock, since it could be modified from
    ;; another thread
    (setf *modified-shader-functions* nil)
    (when recompile
      (format t "~%recompiling shader program for changes in functions:~&  ~a~%"
              m)
      (load-shaders))))


(defvar *u*)
(defvar *once* t)


(defun gl-draw-callback (config draws buffers)
  (recompile-modified-shaders)
  (gl:use-program (shader config))

  (let* ((vbos (gl:gen-buffers (length (getf buffers :vbo-sizes))))
         (ibo (gl:gen-buffer))
         (enabled-atts (make-hash-table))
         (uniformh (uniforms config)))
    (flet ((uv (u v)
             (when uniformh
               (gl:uniformfv (car (gethash u uniformh '(-1))) v))))
      (uv '3b-glim/gl-shaders::dims (dims config)))
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
           (loop for buffer in (3b-glim/s::buffers
                                (3b-glim/s::vertex-format 3b-glim/s::*state*))
                 for stride = (3b-glim/s::stride buffer)
                 for vbo in vbos
                 do (gl:bind-buffer :array-buffer vbo)
                    (loop for att in (3b-glim/s::attributes buffer)
                          for offset = (3b-glim/s::offset att)
                          for normalize = (3b-glim/s::normalized att)
                          for (nil count nil type)
                            = (gethash (3b-glim/s::element-type att)
                                       3b-glim/s::*attribute-types*)
                          for loc = (3b-glim/s::attribute-index att)
                          do (gl:enable-vertex-attrib-array loc)
                             (setf (gethash loc enabled-atts) loc)
                             (%gl:vertex-attrib-pointer
                              loc count type normalize stride offset)))
           ;; dispatch draws
           (loop
             for draw in draws
             for uniforms = (3b-glim/s::uniforms draw)
             for index-base = (3b-glim/s::index-base draw)
             for index-count = (3b-glim/s::index-count draw)
             for primitive = (3b-glim/s::primitive draw)
             for shader-id = (3b-glim/s::shader draw)
             for vertex-base = (3b-glim/s::vertex-base draw)
             for vertex-count = (3b-glim/s::vertex-count draw)
             for enables = (3b-glim/s::enables draw)
             for disables = (3b-glim/s::disables draw)
             do (when (or enables disables)
                  (format t "~&enable ~s~%disable ~s~%" enables disables))
                (mapcar #'gl:enable enables)
                (mapcar #'gl:disable disables)
                (loop
                     for u being the hash-keys of uniforms
                       using (hash-value v)
                     for uu = (gethash u uniformh)
                     for (ui nil nil ut) = uu
                     when (eql u :textures)
                       do (loop
                            for i from 0
                            for tx across v
                            for target in '(:texture-1d :texture-2d :texture-3d)
                            for u in '(3b-glim:tex0-1 3b-glim:tex0-2
                                       3b-glim:tex0-3)
                            when tx
                              do (gl:active-texture i)
                                 (gl:bind-texture target tx)
                                 (when uniformh
                                   (gl:uniformi (car (gethash u uniformh '(-1))) i)))
                     when (and (eql u :lighting)
                               (light-uniforms config))
                       do (loop for i across v
                                for u across (light-uniforms config)
                                when (plusp u)
                                  do (gl:uniformfv u i))
                     do (when (and ui (not (minusp ui)))
                          (ecase ut
                            (:int
                             (gl:uniformi ui v))
                            ((:ivec2 :ivec3 :ivec4)
                             (if (numberp v)
                                 (gl:uniformi ui v 0 0 0)
                                 (gl:uniformiv ui v)))
                            (:float (gl:uniformf ui v))
                            ((:vec2 :vec3 :vec4)
                             (if (numberp v)
                                 (gl:uniformf ui v 0 0 0)
                                 (gl:uniformfv ui v)))
                            (:mat4
                             (gl:uniform-matrix-4fv ui v nil)))))
                (if index-base
                    (when (plusp index-count)
                      (%gl:draw-elements-base-vertex
                       primitive index-count
                       :unsigned-short (* index-base 2)
                       vertex-base))
                    (when (plusp vertex-count)
                      (%gl:draw-arrays primitive vertex-base vertex-count)))))
      ;; clean up
      (gl:use-program 0)
      (map nil 'gl:disable-vertex-attrib-array
           (alexandria:hash-table-keys enabled-atts))
      (gl:delete-buffers (list* ibo vbos)))))

(defun init-state/gl (&key (api :gl))
  (configure-renderer api)
  (load-shaders))

(defmacro with-state/gl ((&key (api :gl) (init t))&body body)
  `(3b-glim:with-state (:draw-callback 'gl-draw-callback)
     (when ,init
       (init-state/gl :api ,api))
     ,@body))

(defmethod 3b-glim::begin-frame ((state glim-gl-state))
  (let ((v (gl:get* :viewport)))
    (setf (dims state)
          (3b-glim::v4 (aref v 0) (aref v 1)
                       (/ (aref v 2)) (/ (aref v 3))))))
