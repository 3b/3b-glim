(in-package #:3b-glim)


(defvar *state*)
;; using octet buffer + nibbles to allow storing things a bit more
;; efficiently.
(deftype octet () '(unsigned-byte 8))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype octet-vector () '(simple-array octet (*)))
(deftype u16-vector () '(simple-array u16 (*)))
(deftype u32-vector () '(simple-array u32 (*)))
(deftype mat4x4 () '(simple-array single-float (16)))
(declaim (type (or null octet-vector) *buffer*))
(defvar *buffer* nil)
(declaim (type (and fixnum unsigned-byte) *buffer-index*))
(defvar *buffer-index* 0)
(declaim (type (or null u16-vector) *index-buffer*))
(defvar *index-buffer* nil)
(declaim (type (and fixnum unsigned-byte) *index-buffer-index*))
(defvar *index-buffer-index* 0)


(declaim (inline f c4 v4))
(defun f (x) (coerce x 'single-float))
(defun c4 (r &optional (g 0.0) (b 0.0) (a 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f r) (f g) (f b) (f a))))
(defun v4 (x &optional (y 0.0) (z 0.0) (w 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z) (f w))))

(defun make-vertex-format (&rest vf &key vertex normal color tex-coord0
                           &allow-other-keys)
  (declare (ignore normal color tex-coord0))
  ;; currently vertex must be 4 elements, and first thing in bufferr
  (assert (= (car vertex) 4))
  (assert (eql (car vf) :vertex))
  (flet ((align (x)                   ; todo: check required alignment
           ;; we always align each component to multiple of 4 bytes
           (* 4 (ceiling x 4))))
    (let ((h (make-hash-table))
          (s 0))
      (loop for (k (v es)) on vf by #'cddr
            do (setf (gethash k h) (list s v))
               (incf s (align (* v es))))
      (list s h))))

;; indices into flags[]
(defconstant +prim-mode-flag+ 0)
(defconstant +edge-flag-flag+ 1)
(defconstant +corner-index-flag+ 2)
;; values for flags[+prim-mode-flag+]
(defconstant +triangle-flag+ 0)
(defconstant +point-flag+ 1)
(defconstant +line-flag+ 2)
;;(defconstant +line-cap-flag+ 3)
;; values for tex-mode* uniforms
(defconstant +tex-mode-off+ 0)
(defconstant +tex-mode-1d+ 1)
(defconstant +tex-mode-2d+ 2)
(defconstant +tex-mode-3d+ 3)
;; indices for draw-flags uniform
(defconstant +draw-flag-mode+ 0)
;; values for +draw-flag-mode+
(defconstant +draw-mode-normal+ 0)
(defconstant +draw-mode-smooth+ 1)
(defconstant +draw-mode-wireframe+ 2) ;; tris/quads only

;; fog coord? texgen?
(defparameter *default-vertex-format*
  ;; fixme: separate size from format, or store type too so writer
  ;; will match. for now assuming float for vert/tex/normal/color, u8 for flags
  '(:vertex (4 4)
                                        ;:fog-coord (1 4)
    :texture0 (4 4)
    :texture1 (4 4)
    ;; only supporting 2 textures for now, probably don't even need that
                                        ;:texture2 (4 4)
                                        ;:texture3 (4 4)
    :color (4 4)
    :normal (3 4)
    ;; stores extra data for generating line vertices
    :tangent+width (4 4)
    :secondary-color (3 4) ;; alpha is always 1
    ;; 0 = primitive type: see +trangle-flag+ etc above
    ;; 1 = edge flag where applicable
    ;; 2 = corner index for vertex-shader points/lines
    :flags (4 1)))
(defparameter *default-vertex* (apply #'make-vertex-format
                                      *default-vertex-format*))
;; default buffer size in vertices (so actual size is probably 60-100x)
;; 1.64MB with current defaults
(defparameter *default-buffer-size* (expt 2 14))

(defun make-flags ()
  (alexandria:plist-hash-table
   '(:line-smooth nil
     :point-smooth nil)))

(defclass glim-state ()
  ;; size of buffer in vertices
  ((buffer-size :initform *default-buffer-size* :reader buffer-size)
   ;; parameters of current begin/end
   (primitive :reader primitive :initform nil :writer (setf %primitive))
   ;; parameters of completed chunks (either a begin/end pair, or from
   ;; begin to buffer overflow)
   (draws :reader draws :initform (make-array 32 :adjustable t :fill-pointer 0))
   ;; array containing current vertex state
   (current-vertex :reader current-vertex
                   :initarg :current-vertex
                   :initform (make-array (first *default-vertex*)
                                         :element-type 'octet
                                         :initial-element 0))
   (quad-temp :accessor quad-temp :initform nil)
   ;; size of vertex in floats
   (vertex-size :accessor vertex-size :initform (first *default-vertex*)
                :initarg :vertex-size)
   ;; offsets/size of vertex attributes in current-vertex
   (vertex-format :reader vertex-format :initform (second *default-vertex*)
                  :initarg :format)
   (flags :reader flags :initform (make-flags))
   (point-size :accessor current-point-size :initform 1.0)
   (line-width :accessor current-line-width :initform 1.0)
   (use-tess :reader use-tess :initform nil :writer (setf %use-tess))
   (renderer-config :reader renderer-config :initform nil
                    :writer (setf %renderer-config))
   ;; fixme: match GL defaults
   (shade-model :accessor %shade-model :initform :smooth)

   (light-model :reader %light-model :initform
                (alexandria:plist-hash-table
                 `(:light-model-local-viewer 1)))
   (color-material :reader %color-material
                   :initform (alexandria:plist-hash-table
                              `(:front :ambient-and-diffuse
                                :back :ambient-and-diffuse)))
   ;; if set, called when buffer fills up, or at end of frame. passed
   ;; 1 argument containing draws since last call to draw callback or
   ;; to get-draws
   (draw-callback :reader draw-callback :initform nil :initarg :draw-callback)))


(defun get-draws ()
  (prog1
      (copy-seq (draws *state*))
    ;; clear old values so they aren't kept live
    (fill (draws *state*) nil)
    ;; and reset fill pointer
    (setf (fill-pointer (draws *state*)) 0)))

(defun maybe-call-draw-callback ()
  (when (and (draw-callback *state*)
             (plusp (length (draws *state*))))
    (funcall (draw-callback *state*)
             (get-draws))))


(defmethod (setf point-size) :before (n (s glim-state))
  ;; not valid inside BEGIN/END
  (assert (not (primitive s))))

(defmethod (setf line-width) :before (n (s glim-state))
  ;; not valid inside BEGIN/END
  (assert (not (primitive s))))


(declaim (inline current-vertex-index out*))
(defun current-vertex-index ()
  (let ((vs (vertex-size *state*))
        (s (fourth (primitive *state*))))
    (declare (fixnum vs s))
    (/ (- *buffer-index* s)
       vs)))

(defun out* (from-buffer offset)
  (let* ((s (vertex-size *state*))
         (e (+ *buffer-index* s)))
    (declare (fixnum e s))
    (when *buffer*
      (locally (declare (type octet-vector *buffer*))
        (replace *buffer* from-buffer :start1 *buffer-index* :end1 e
                                      :start2 offset)))
    ;; return index of vertex in draw
    (prog1
        (current-vertex-index)
      (setf *buffer-index* e))))

(declaim (inline outi))
(defun outi (index)
  (setf (aref *index-buffer* *index-buffer-index*) index)
  (incf *index-buffer-index*))

(defun notice-state-changed ())
(defun enable (&rest flags)
  (loop with h = (flags *state*)
        with state-changed = nil
        for f in flags
        do (multiple-value-bind (old found) (gethash f h)
             (if found
                 (setf (gethash f h) t
                       state-changed (or state-changed (not old)))
                 (gl:enable f)))
        finally (when state-changed
                  (notice-state-changed))))

(defun disable (&rest flags)
  (loop with h = (flags *state*)
        with state-changed = nil
        for f in flags
        do (multiple-value-bind (old found) (gethash f h)
             (if found
                 (setf (gethash f h) nil
                       state-changed (or state-changed old))
                 (gl:disable f)))
        finally (when state-changed
                  (notice-state-changed))))

(defmethod initialize-instance :after ((o glim-state) &key)
  (let ((h (vertex-format o))
        (*state* o))
    (when (gethash :normal h)
      (%normal 0.0 0.0 1.0))
    (when (gethash :color h)
      (%color 1.0 1.0 1.0 1.0))
    (when (gethash :secondary-color h)
      (%secondary-color 0.0 0.0 0.0))
    #++(when (gethash :fog h)
         (fog-coord 0.0))
    (when (gethash :texture0 h)
      (%multi-tex-coord 0 0.0 0.0 0.0 1.0))
    (when (gethash :texture1 h)
      (%multi-tex-coord 1 0.0 0.0 0.0 1.0))
    (when (gethash :texture2 h)
      (%multi-tex-coord 2 0.0 0.0 0.0 1.0))
    (when (gethash :texture3 h)
      (%multi-tex-coord 3 0.0 0.0 0.0 1.0))))

(defun has-space-for-vertices (n)
  (< (+ *buffer-index* (* n (vertex-size *state*)))
     (length *buffer*)))
(defun has-space-for-indices (n)
  (< (+ *index-buffer-index* n)
     (length *buffer*)))

(defmacro with-state ((&key draw-callback) &body body)
  `(let ((*state* (make-instance 'glim-state
                                 :draw-callback ,draw-callback))
         ;; add option to use a single static buffer instead of allocating
         ;; per frame?
         (*matrix-stacks* (make-hash-table))
         (*matrices* (make-hash-table)))
     (ensure-matrix :modelview)
     (ensure-matrix :projection)
     ,@body))

(defun reset-buffers ()
  (setf *buffer* (make-array (* (vertex-size *state*)
                                (buffer-size *state*))
                             :element-type 'octet))
  (setf *buffer-index* 0)
  (setf *index-buffer* (make-array (* 3 (buffer-size *state*))
                                   :element-type '(unsigned-byte 16)))
  (setf *index-buffer-index* 0))

(defmethod begin-frame ((state glim-state))
  (reset-buffers)
  (begin-frame (second (renderer-config state))))

(defmacro with-frame (() &body body)
  `(let ((*buffer* nil)
         (*buffer-index* 0)
         (*index-buffer* nil)
         (*index-buffer-index* 0))
     (begin-frame *state*)
     (prog1
         (with-balanced-matrix-stacks () ,@body)
       (maybe-call-draw-callback))))

;; define with and without S to match cl-opengl
(defmacro with-primitives (primitive &body body)
  `(with-primitive ,primitive ,@body))

(defmacro with-primitive (primitive &body body)
  `(progn
     (begin ,primitive)
     (unwind-protect
          (prog1
              (progn ,@body)
            ;; on normal exit, we call END to finish chunk
            (end))
       ;; and we clear primitive even on nlx so next begin doesn't fail
       (setf (%primitive *state*) nil))))

(defvar *primitives* (alexandria:plist-hash-table
                      ;; min # of vertices to finish a primitive
                      '(:points 1
                        :lines 2
                        :triangles 3
                        :quads 6 ;; 2 tris
                        :line-strip 2
                        ;; todo: :line-loop 2
                        :triangle-strip 3
                        :triangle-fan 3
                        :quad-strip 6 ;; 2 tris
                        ;; todo:
                        ;; :polygon ?
                        )))


(defun copy-partial (primitive from-buffer start end)
                                        ;(declare (notinline out*))
  (case primitive
    (:line-strip
     ;; don't need to copy if we are emitting lines as tris
     (when (or (use-tess *state*)
               (and (= (current-line-width *state*) 1)
                    (not (gethash :line-smooth (flags *state*)))))
       (assert (has-space-for-vertices 1))
       ;; copy previous element
       (out* from-buffer (- end (vertex-size *state*)))))
    (:triangle-fan
     (assert (has-space-for-vertices 2))
     ;; copy first element and previous element
     (out* from-buffer start)
     (out* from-buffer (- end (vertex-size *state*))))
    (:triangle-strip
     (assert (has-space-for-vertices 2))
     ;; copy previous 2 elements
     (out* from-buffer (- end (* 2 (vertex-size *state*))))
     (out* from-buffer (- end (vertex-size *state*))))
    ;; quad-strip is always turned into tris

    (:quad-strip
     (assert (has-space-for-vertices 2))
     ;; copy previous 2 elements
     (out* from-buffer (- end (* 2 (vertex-size *state*))))
     (out* from-buffer (- end (vertex-size *state*))))))

(defun finish-chunk ()
  (destructuring-bind (primitive size buffer start
                       index-buffer index-start)
      (primitive *state*)
    (declare (ignorable primitive size))
    (let ((i (current-vertex-index))
          (base (/ start (vertex-size *state*))))
      ;; skip draw if we don't have a full primitive
      (when (>= i 3)
        ;; for now drawing everything as triangles
        (vector-push-extend
         (list :triangles
               :buffer buffer
               ;; not sure these are needed, but might be able to
               ;; upload less
               :start start :end *buffer-index*
               :base-index base
               :index-buffer index-buffer
               :start-index index-start
               :index-count
               ;; skip any partial triangles at end
               (* 3
                  (floor (- *index-buffer-index* index-start)
                         3))
               :uniforms
               ;; fixme: build this once and only store changes?
               (list
                'mv (copy-seq (ensure-matrix :modelview))
                'proj (copy-seq (ensure-matrix :projection))
                'line-width (current-line-width *state*)
                'point-size (current-point-size *state*)
                'tex-mode0 +tex-mode-off+
                'tex-mode1 +tex-mode-off+
                'tex0-1 0
                'tex1-1 0
                'tex0-2 0
                'tex1-2 0
                'tex0-3 0
                'tex1-3 0
                'light-postion (v4 0 3 0 1)
                ;; todo
                ;; 'normal-matrix (??)
                ))
         (draws *state*)))
      (setf (%primitive *state*) nil))))

(defun set-primitive (primitive size &optional (start *buffer-index*)
                                       (istart *index-buffer-index*))
  (setf (%primitive *state*)
        (list primitive size *buffer* start *index-buffer* istart)))


(defun overflow ()
  (assert (primitive *state*))
  (let ((prim (primitive *state*))
        (end *buffer-index*))
    (finish-chunk)
    (reset-buffers)
    (maybe-call-draw-callback)
    (destructuring-bind (primitive size buffer old-start
                         index-buffer old-index-start)
        prim
      (declare (ignore index-buffer old-index-start))
      (let ((start *buffer-index*)
            (istart *index-buffer-index*))
        (set-primitive primitive size start istart)
        (copy-partial primitive buffer old-start end)
        (set-primitive primitive size start istart)))))

(defun begin (primitive)
  #++(declare (optimize speed))
  (let ((prim-size (gethash primitive *primitives*))
        (o (car (gethash :flags (vertex-format *state*))))
        (cv (current-vertex *state*)))
    (ecase primitive
      (:points (setf (aref cv (+ o +prim-mode-flag+)) +point-flag+))
      ((:lines :line-strip)
       (setf (aref cv (+ o +prim-mode-flag+)) +line-flag+))
      ((:triangles :triangle-fan :triangle-strip :quads :quad-strip)
       (setf (aref cv (+ o +prim-mode-flag+)) +triangle-flag+)))
    ;; todo: error messages
    (assert prim-size)
    (assert (not (primitive *state*)))
    (unless (and (has-space-for-vertices 4)
                 (has-space-for-indices 4))
      (reset-buffers))
    (set-primitive primitive prim-size)))

(defun end ()
  (assert (primitive *state*))
  (finish-chunk))

;;; w/tess
;;; point: store/submit as point, expand to 2 tris
;;; line: store/submit as line, expand to 2 tris
;;; line-strip: store/submit as lines, expand to 2 tris
;;; line-loop = ?
;;; triangles: as is
;;; triangle strip: as is
;;; triangle fan: as is
;;; quads: store/submit as 4-patch, output quad?
;;; quad-strips: like quads?


;;; w/o tess:

;;; point: store/submit as 2 tris (= 4 verts)
;; = 1 vertex repeated 4 times, with flag[2] = 0-3 to indicate which
;;  vertex. set edge flags for outer edges if set for vertex?

;;; line/line-strip: store/submit as 2 tris (= 4 verts) per quad
;; = 2 vertices, repeated 2 times each, with flag[1] = 0-3 to indicate
;;  which vertex? dir between 2 points stored in tangent[]. Using
;;  round end caps, so don't need to care about adjacent lines in
;;  line-strip (set edge flags for outer edges if set for vertex?)

;;; line-loop: ?
;; = not implemented for now...

;;; triangles:
;; stored as-is

;;; triangle-strip:
;; expand to separate triangles w/shared indices

;;; quads:
;; expand to 2 triangles, set edge flag to 0 for generated edge

;;; quad-strip:
;; same as quads?




(defun write-vertex-line (cv)
  (declare (optimize speed)
           (type octet-vector cv))
  ;; todo: tess and non-smooth width 1 lines
  #++
  (or (use-tess *state*)
      (and (= (current-line-width *state*) 1)
           (not (gethash :line-smooth (flags *state*)))))
  (assert *buffer*)
  (destructuring-bind (o1 s)
      (gethash :tangent+width (vertex-format *state*))
    (declare (ignore s) (type u16 o1))
    (let ((o (+ o1 (* 3 4))))
      (setf (nibbles:ieee-single-ref/le cv o) (current-line-width *state*))))
  (let* ((index (current-vertex-index))
         ;; we write 1 vertex for start of line, 3 more for end, so if
         ;; index is a multiple of 4 we are starting a line, otherwise
         ;; ending a line
         (end (plusp (mod index 4)))
         (vs (vertex-size *state*)))

    (declare (fixnum index)
             (type u16 vs)
             (type octet-vector *buffer*))
    (if end
        ;; store opposite end in each point, for calculating wide lines
        ;; todo: rename 'tangent' uniform now that it is opposite end
        (let* ((vf (vertex-format *state*))
               (s (- *buffer-index* vs))
               (to (first (gethash :tangent+width vf)))
               (fo (first (gethash :flags vf)))
               (sv (sb-cga:vec
                    ;; position is at start of vertex, so no extra offset
                    (nibbles:ieee-single-ref/le *buffer* (+ s 0))
                    (nibbles:ieee-single-ref/le *buffer* (+ s 4))
                    (nibbles:ieee-single-ref/le *buffer* (+ s 8))))
               (ev (sb-cga:vec
                    (nibbles:ieee-single-ref/le cv 0)
                    (nibbles:ieee-single-ref/le cv 4)
                    (nibbles:ieee-single-ref/le cv 8))))
          (declare (type u32 s)
                   (type u16 fo to))
          (loop for i below 3
                do (setf (nibbles:ieee-single-ref/le *buffer*
                                                     (+ s to (* i 4)))
                         (aref ev i))
                   (setf (nibbles:ieee-single-ref/le cv (+ to (* i 4)))
                         (aref sv i)))

          ;; set corner index in flags of start vertex
          (setf (aref *buffer* (+ s fo +corner-index-flag+)) 0)
          ;; then write remaining 3 vertices and indices
          (let ((s2 *buffer-index*))
            ;; duplicate first vertex
            (out* *buffer* (- *buffer-index* vs))
            ;; set index in flags of 2nd vertex
            (setf (aref *buffer* (+ s2 fo +corner-index-flag+)) 1))
          ;; write 2nd vertex twice with different corner index
          (setf (aref cv (+ fo +corner-index-flag+)) 2)
          (out* cv 0)
          (setf (aref cv (+ fo +corner-index-flag+)) 3)
          (let ((i (out* cv 0)))
            (declare (fixnum i))
            ;; write index buffer
            (outi (- i 3))
            (outi (- i 2))
            (outi (- i 1))

            (outi (- i 3))
            (outi (- i 1))
            (outi (- i 0))))
        ;; for start of line, just write 1 vertex
        (out* cv 0))
    ;; if we ended a line, make sure we have enough space for 4 more
    ;; verts and 6 indices
    (when (and end (or (not (has-space-for-vertices 4))
                       (not (has-space-for-indices 6))))
      (overflow))))

(defun write-vertex-line-strip (cv)
  ;; todo: tess and non-smooth width 1 lines
  (let* ((index (current-vertex-index)))
    ;; after first segment is done, we copy a vertex
    (when (>= index 2)
      (out* *buffer* (- *buffer-index* (vertex-size *state*))))
    ;; and let line code handle the rest
    (write-vertex-line cv)))

(defun write-vertex-point (cv)
  (declare (optimize speed)
           (type octet-vector cv))
  ;; todo: tess path, non-smooth size=1
  ;; store width in 4th element of tangent
  (destructuring-bind (o1 s)
      (gethash :tangent+width (vertex-format *state*))
    (declare (type (unsigned-byte 16) o1 s))
    (let ((o (+ o1 (* 3 s))))
      (setf (nibbles:ieee-single-ref/le cv o) (current-point-size *state*))))
  (let ((o (car (gethash :flags (vertex-format *state*)))))
    (declare (type (unsigned-byte 16) o))
    (setf (aref cv (+ o +prim-mode-flag+)) +point-flag+)

    (let* ((v (loop with o = (+ +corner-index-flag+ o)
                    for i below 4
                    do (setf (aref cv o) i)
                    collect (out* cv 0))))

      (outi (first v))
      (outi (second v))
      (outi (third v))
      (outi (first v))
      (outi (third v))
      (outi (fourth v))))
  (unless (has-space-for-vertices 4)
    (overflow)))

(defun write-vertex-quad (cv)
  (declare (optimize speed)
           (type octet-vector cv))
  (let* ((i (out* cv 0))
         (end (= 3 (mod i 4))))
    (declare (type (unsigned-byte 16) i))
    ;; at end of quad, write indices
    (when end
      (outi (- i 3))
      (outi (- i 2))
      (outi (- i 1))

      (outi (- i 3))
      (outi (- i 1))
      (outi (- i 0))
      ;; and make sure we have enough space for 4 more verts and 6
      ;; indices
      (unless (and (has-space-for-vertices 4)
                   (has-space-for-indices 6))
        (overflow)))))

(defun write-vertex-quad-strip (cv)
  ;; for a quad strip, we need to duplicate previous 2 indices when we
  ;; start a new quad after first
  (let* ((index (current-vertex-index))
         (new (and (> index 4)
                   (zerop (mod index 4)))))
    (when new
      (let ((d (* 2 (vertex-size *state*))))
        (out* *buffer* (- *buffer-index* d))
        (out* *buffer* (- *buffer-index* d))))
    ;; rest is handled by normal quad code
    (write-vertex-quad cv)))


(defun write-vertex ()
  (declare (optimize speed))
  (let ((primitive (car (primitive *state*)))
        (cv (current-vertex *state*)))
    (ecase primitive
      ((:triangles)
       (let ((index (out* cv 0)))
         (outi index)
         (when (and (zerop (mod (1+ index) 3))
                    (not (has-space-for-vertices 3)))
           (overflow))
         (unless (has-space-for-indices 3)
           (overflow))))
      (:triangle-strip
       (let ((i (out* cv 0)))
         ;; index 0 and 1 don't make a triangle, otherwise copy 2 indices
         (when (> i 1)
           (multiple-value-bind (f r) (floor i 2)
             (outi (1- (* f 2)))
             (outi (* 2 (+ f (- r 1))))))
         (outi i)
         (when (has-space-for-vertices 1)
           (overflow))
         (when (and (> i 1) (not (has-space-for-indices 3)))
           (overflow))))
      (:triangle-fan
       (let ((i (out* cv 0)))
         ;; index 0 and 1 don't make a triangle, otherwise copy 2 indices
         (when (> i 1)
           (outi 0)
           (outi (1- i)))
         (outi i)
         (when (has-space-for-vertices 1)
           (overflow))
         (when (and (> i 1) (not (has-space-for-indices 3)))
           (overflow))))
      (:points
       (write-vertex-point cv))
      ;; :lines and line-strips are converted to quads if size /= 1 or if
      ;;  smooth is on
      (:lines
       (write-vertex-line cv))
      (:line-strip
       (write-vertex-line-strip cv))
      (:line-loop
       ;; todo: needs to store state extra state across chunks to close
       ;; loop
       )
      ;; :quads and :quad-strip are converted to tris / tri-strips
      (:quads
       (write-vertex-quad cv))
      (:quad-strip
       (write-vertex-quad-strip cv)))))


(defun %vertex (x y z w)
  (declare (optimize speed))
  (let ((s (gethash :vertex (vertex-format *state*))))
    (assert s)
    (destructuring-bind (o c) s
      (check-type o (unsigned-byte 30))
      (check-type c fixnum)
      (assert (= c 4))
      (let ((v (current-vertex *state*)))
        ;; fixme: endianness?
        (setf (nibbles:ieee-single-ref/le v (+ o 0)) x)
        (setf (nibbles:ieee-single-ref/le v (+ o 4)) y)
        (setf (nibbles:ieee-single-ref/le v (+ o 8)) z)
        (setf (nibbles:ieee-single-ref/le v (+ o 12)) w)))
    (write-vertex)))
(declaim (inline vertex))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (%vertex (f x) (f y) (f z) (f w)))

(defun %normal (x y z)
  (let ((s (gethash :normal (vertex-format *state*))))
    (when s
      (destructuring-bind (o c) s
        (assert (= c 3))
        (let ((v (current-vertex *state*)))
          (setf (nibbles:ieee-single-ref/le v (+ o 0)) x)
          (setf (nibbles:ieee-single-ref/le v (+ o 4)) y)
          (setf (nibbles:ieee-single-ref/le v (+ o 8)) z))))))
(declaim (inline normal))
(defun normal (x y z)
  (%normal (f x) (f y) (f z)))

(defun edge-flag (flag)
  (let ((s (gethash :flags (vertex-format *state*))))
    (when s
      (let ((v (current-vertex *state*)))
        (setf (aref v (+ (car s) 0)) (if flag 1 0))))))

(defun %multi-tex-coord (tex s tt r q)
  (declare (optimize speed))
  (when (numberp tex)
    (setf tex (aref #(:texture0 :texture1 :texture2 :texture3) tex)))
  (let* ((spec (gethash tex (vertex-format *state*))))
    (when spec
      (destructuring-bind (o c) spec
        (check-type o (unsigned-byte 30))
        (check-type c fixnum)
        (let ((v (current-vertex *state*)))
          (setf (nibbles:ieee-single-ref/le v (+ o 0)) s)
          (when (> c 1) (setf (nibbles:ieee-single-ref/le v (+ o 4)) tt))
          (when (> c 2) (setf (nibbles:ieee-single-ref/le v (+ o 8)) r))
          (when (> c 3) (setf (nibbles:ieee-single-ref/le v (+ o 12)) q)))))))

(declaim (inline multi-tex-coord tex-coord))
(defun multi-tex-coord (tex s &optional (tt 0.0) (r 0.0) (q 1.0))
  (%multi-tex-coord tex (f s) (f tt) (f r) (f q)))
(defun tex-coord (s &optional (tt 0.0) (r 0.0) (q 1.0))
  (%multi-tex-coord 0 (f s) (f tt) (f r) (f q)))

#++
(defun fog-coord (x))
(defun %color (r g b a)
  (declare (optimize speed))
  (let* ((spec (gethash :color (vertex-format *state*))))
    (when spec
      (destructuring-bind (o c) spec
        (check-type o (unsigned-byte 30))
        (check-type c fixnum)
        (let ((v (current-vertex *state*)))
          (assert (= c 4))
          (setf (nibbles:ieee-single-ref/le v (+ o 0))  r)
          (setf (nibbles:ieee-single-ref/le v (+ o 4))  g)
          (setf (nibbles:ieee-single-ref/le v (+ o 8))  b)
          (setf (nibbles:ieee-single-ref/le v (+ o 12)) a))))))

(declaim (inline color))
(defun color (r g b &optional (a 1.0))
  (%color (f r) (f g) (f b) (f a)))


(defun %secondary-color (r g b)
  (let* ((spec (gethash :secondary-color (vertex-format *state*))))
    (when spec
      (destructuring-bind (o c) spec
        (let ((v (current-vertex *state*)))
          (assert (= c 3))
          (setf (nibbles:ieee-single-ref/le v (+ o 0)) r)
          (setf (nibbles:ieee-single-ref/le v (+ o 4)) g)
          (setf (nibbles:ieee-single-ref/le v (+ o 8)) b))))))
(declaim (inline secondary-color))
(defun secondary-color (r g b)
  (%secondary-color (f r) (f g) (f b)))


(defun shade-model (model)
  (setf (%shade-model *state*) model))
(defun light-model (a b)
  ;; fixme: error checking
  (setf (gethash a (%light-model *state*)) b))
(defun color-material (a b)
  ;; fixme: error checking
  (setf (gethash a (%color-material *state*)) b))


(defun map-draws (fun draws)
  (loop for draw across draws
        do (apply fun draw)))

(defun line-width (w)
  (setf (current-line-width *state*) (coerce w 'single-float)))

(defun point-size (w)
  (setf (current-point-size *state*) (coerce w 'single-float)))

