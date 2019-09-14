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


(defclass glim-gl-state ()
  ((shader :accessor shader :initarg :shader)
   (uniforma :accessor uniforms :initform (make-hash-table))
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
         (not (eql api (car (3b-glim::renderer-config 3b-glim:*state*)))))
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
          (list
           api
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
                  (t :simple))))))))

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


(defun load-shaders ()
  "Loads shaders used by glim into current GL context. Must call
CONFIGURE-RENDERER or CONFIGURE-RENDERER* first."
  (let* ((cfg (3b-glim::renderer-config 3b-glim:*state*))
         (api (first cfg))
         (config (second cfg)))
    (assert (member api '(:gl :gles)))
      (time
       (setf (values (shader config)
                     (uniforms config))
             (3bgl-shaders::reload-program
              (shader config)
              '3b-glim/gl-shaders:vertex
              '3b-glim/gl-shaders:fragment
              :version 330)))))

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

(defun gl-draw-callback (draws)
  (let* ((cfg (3b-glim::renderer-config 3b-glim:*state*))
         (api (first cfg))
         (config (second cfg)))
    (assert (member api '(:gl :gles)))
    (recompile-modified-shaders)
    (gl:use-program (shader config))
    (setf *u* (list config))
    (let* ((vbo (gl:gen-buffer))
           (ibo (gl:gen-buffer))
           (last-vb (list nil 0))
           (last-ib (list nil 0))
           (batches nil)
           (itype :unsigned-short)
           (isize 2)
           (uniformh (uniforms config)))
      (labels ((uv (u v)
                 (gl:uniformfv (car (gethash u uniformh '(-1))) v))
               (uniforms (uniforms)
                    (loop for (u v) on uniforms by #'cddr
                          for uu = (gethash u uniformh)
                          for (ui nil nil ut) = uu
                          do (when (and ui (not (minusp ui)))
                               (ecase ut
                                 (:float (gl:uniformf ui v))
                                 ((:vec2 :vec3 :vec4)
                                  (gl:uniformfv ui v))
                                 (:mat4 (gl:uniform-matrix-4fv ui v nil)))))
                    )
               (draw ()
                 (when batches
                   (setf itype
                         (etypecase (car last-ib)
                           (3b-glim::u16-vector :unsigned-short)
                           (3b-glim::u32-vector :unsigned-int)))
                   (setf isize
                         (etypecase (car last-ib)
                           (3b-glim::u16-vector 2)
                           (3b-glim::u32-vector 4)))
                   (gl:bind-buffer :array-buffer vbo)
                   (cffi:with-pointer-to-vector-data (p (car last-vb))
                     (when *once*
                       (format t "upload vbo ~s~%" (cadr last-vb)))
                     (%gl:buffer-data :array-buffer (cadr last-vb)
                                      p :stream-draw))
                   (gl:bind-buffer :element-array-buffer ibo)
                   (cffi:with-pointer-to-vector-data (p (car last-ib))
                     (when *once*
                       (format t "upload ibo ~s~%" (cadr last-ib)))
                     (%gl:buffer-data :element-array-buffer
                                      (* (cadr last-ib)
                                         isize)
                                      p :stream-draw))
                   (let ((s (3b-glim::vertex-size 3b-glim::*state*))
                         (c (3b-glim::vertex-format 3b-glim::*state*)))
                     (flet ((a (i n a &optional (type :float))
                              (%gl:vertex-attrib-pointer i n type nil
                                                         s
                                                         (car (gethash a c)))
                              (gl:enable-vertex-attrib-array i)))
                       (a 0 4 :vertex)
                       (a 3 4 :color)
                       (a 5 4 :tangent+width)
                       (a 7 4 :flags :byte)))
                   (loop for (p base start count uniforms)
                           in (nreverse (shiftf batches nil))
                         do (uniforms uniforms)
                            (when *once*
                              (format t "draw ~s ~s @ ~s~%" p count base))
                            (%gl:draw-elements-base-vertex
                             p count
                             itype (* start isize)
                             base)))))
        (uv '3b-glim/gl-shaders::dims (dims config))
        (3b-glim:map-draws
         (lambda (prim &key buffer start end base-index index-buffer
                         start-index index-count
                         uniforms)
           (declare (ignore start))
           (unless (and (vectorp buffer)
                        (vector index-buffer))
             (break "?"))
           (when *once*
             (format t "map ~s ~s ~s ~s~%" prim end base-index index-count))
           (unless (and (eql buffer (car last-vb))
                        (eql index-buffer (car last-ib)))
             (draw) (setf batches nil)
             (setf (car last-vb) buffer
                   (cadr last-vb) end
                   (car last-ib) index-buffer
                   (cadr last-ib) (+ start-index index-count)))
           (setf (cadr last-vb) (max (cadr last-vb) end)
                 (cadr last-ib) (max (cadr last-ib)
                                     (+ start-index index-count)))
           (push (list prim base-index start-index index-count
                       uniforms)
                 batches))
         draws)
        (draw))
      (gl:delete-buffers (list vbo ibo)))
    (setf *once* nil)
    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 2)
    (gl:disable-vertex-attrib-array 3)
    (gl:disable-vertex-attrib-array 4)
    (gl:disable-vertex-attrib-array 5)
    (gl:disable-vertex-attrib-array 6)
    (gl:disable-vertex-attrib-array 7)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :element-array-buffer 0)
    (gl:use-program 0)))

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
                       (/ (aref v 2)) (/ (aref v 3)))))
)
