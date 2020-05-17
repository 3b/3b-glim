(in-package #:3b-glim)


(defconstant +position+ 0)
(defconstant +texture0+ 1) ;; only supporting 1 textures for now
(defconstant +tangent-width+ 2)
(defconstant +normal+ 3)
(defconstant +color+ 4)
(defconstant +flags+ 5)
(defconstant +secondary-color+ 6)


(defvar *format*
  (3b-glim/s:compile-vertex-format
   `(1
     (,+position+ :vec4)
     (,+texture0+ :vec4)
     (,+tangent-width+ :vec4)
     (,+normal+ :vec3)
     (,+color+ :u8vec4)
     ;; stores extra data for generating line vertices
     ;; 0 = primitive type: see +trangle-flag+ etc above
     ;; 1 = edge flag where applicable
     ;; 2 = corner index for vertex-shader points/lines

     (,+flags+ (:u8vec4 nil))
     (,+secondary-color+ :u8vec3)
     ))) ;; color




;; indices into flags[]
(defconstant +prim-mode-flag+ 0)
(defconstant +edge-flag-flag+ 1)
(defconstant +corner-index-flag+ 2)
;; values for flags[+prim-mode-flag+]
(defconstant +triangle-flag+ 0)
(defconstant +point-flag+ 1)
(defconstant +line-flag+ 2)
(defconstant +quad-flag+ 3)
;;(defconstant +line-cap-flag+ 3)
;; values for tex-mode* uniforms
(defconstant +tex-mode-off+ 0)
(defconstant +tex-mode-1d+ 1)
(defconstant +tex-mode-2d+ 2)
(defconstant +tex-mode-3d+ 3)
;; indices for draw-flags uniform
(defconstant +draw-flag-mode+ 0)
(defconstant +draw-flag-mode-back+ 1)
(defconstant +draw-flag-lighting+ 2)
;; values for +draw-flag-mode+
(defconstant +draw-mode-normal+ 0)
(defconstant +draw-mode-smooth+ 1)
(defconstant +draw-mode-wireframe+ 2)        ;; tris/quads only
(defconstant +draw-mode-filled-wireframe+ 3) ;; tris/quads only
;; lighting stuff
(defconstant +max-lights+ 4)

(defun make-flags ()
  (alexandria:plist-hash-table
   '(:line-smooth nil
     :point-smooth nil
     :wireframe nil
     :filled-wireframe nil
     :texture-1d nil
     :texture-2d nil
     :texture-3d nil
     :light0 nil
     :light1 nil
     :light2 nil
     :light3 nil
     :lighting nil)))

(defstruct (light (:type vector))
  (position (v4 0 0 1 0))
  (ambient (v4 0 0 0 1))
  (diffuse (v4 0 0 0 1))
  (specular (v4 0 0 0 1))
  (spot-dir (v3 0 0 -1))
  (spot-params (v3 0 180 0))
  (attenuation (v3 1 0 0)))

(defconstant +state-changed-line-width+ 0)
(defconstant +state-changed-point-size+ 1)
(defconstant +state-changed-draw-flags+ 2)
(defconstant +state-changed-tex+ 3)
(defconstant +state-changed-lighting+ 4)


(defclass renderer-config ()
  ())
(defclass glim-state (3b-glim/s::writer-state)
  (;; parameters of current begin/end
   (primitive :reader primitive :initform nil :writer (setf %primitive))
   (flags :reader flags :initform (make-flags))
   (prim-flags :reader prim-flags
               :initform (make-array 4 :element-type 'octet
                                       :initial-element 0))
   (point-size :accessor current-point-size :initform 1.0)
   (line-width :accessor current-line-width :initform 1.0)
   (use-tess :reader use-tess :initform nil :writer (setf %use-tess))
   (renderer-config :reader renderer-config :initform nil
                    :accessor %renderer-config)
   ;; extra space to remember previous vertices, used to convert
   ;; strip/loop/quad primitives into separate tris
   (primitive-temp :reader primitive-temp :initarg :primitive-temp)
   ;; for line/strip/fan primitives, we need to track which part of a
   ;; primitive we are drawing, 0 is start of batch, other values
   ;; depends on type
   (primitive-state :accessor primitive-state :initform 0)
   ;; store position of previous points in line so we can calculate tangent
   (line-temp :accessor line-temp :initform nil)
   ;; fixme: match GL defaults
   (shade-model :accessor %shade-model :initform :smooth)
   (light-model :reader %light-model :initform
                (alexandria:plist-hash-table
                 `(:light-model-local-viewer 1)))
   (color-material :reader %color-material
                   :initform (alexandria:plist-hash-table
                              `(:front :ambient-and-diffuse
                                :back :ambient-and-diffuse)))
   (lights :reader lights
           :initform (vector
                      (make-light :diffuse (v4 1 1 1 1) :specular (v4 1 1 1 1))
                      (make-light)
                      (make-light)
                      (make-light)))
   (draw-flags :accessor draw-flags :initform #(0 0 0 0))
   (textures :reader textures :initform (make-array '(3) :initial-element nil))
   (state-changed-flags :accessor state-changed-flags :initform -1)
   ;; if set, called when buffer fills up, or at end of frame. passed
   ;; 1 argument containing draws since last call to draw callback or
   ;; to get-draws
   (draw-callback :reader draw-callback :initform nil :initarg :draw-callback)))

(defmacro with-state ((&key draw-callback) &body body)
  `(3b-glim/s:with-state (*format*)
     (change-class *state* 'glim-state
                   :draw-callback ,draw-callback
                   :primitive-temp
                   (coerce (loop repeat 4
                                 collect (3b-glim/s::copy-current-vertex
                                          *state*))
                           'vector))
     (with-matrix-stacks ()
       (ensure-matrix :modelview)
       (ensure-matrix :projection)
       ,@body)))

(defmethod (setf point-size) :before (n (s glim-state))
  ;; not valid inside BEGIN/END
  (assert (not (primitive s))))

(defmethod (setf line-width) :before (n (s glim-state))
  ;; not valid inside BEGIN/END
  (assert (not (primitive s))))


(defun get-flag (flag)
  (gethash flag (flags *state*)))

(defun get/clear-changed-flag (state change)
  (plusp (shiftf (ldb (byte 1 change) (state-changed-flags state))
                 0)))

(defun notice-state-changed (state change)
  (setf (ldb (byte 1 change) (state-changed-flags state)) 1))

(defun notice-flag-changed (state flag)
  (case flag
    ((:lighting :light0 :light1 :light2 :light4)
     (notice-state-changed state +state-changed-lighting+)
     ;; lighting enable is in draw flags, so update that too
     (notice-state-changed state +state-changed-draw-flags+))
    ((:line-smooth :wireframe :point-smooth :filled-wireframe)
     (notice-state-changed state +state-changed-draw-flags+))
    ((:texture-1d :texture-2d :texture-3d)
     (notice-state-changed state +state-changed-tex+))))

(defun enable (&rest flags)
  (loop with h = (flags *state*)
        for f in flags
        do (multiple-value-bind (old found) (gethash f h)
             (if found
                 (progn
                   (unless old
                     (notice-flag-changed *state* f))
                   (setf (gethash f h) t))
                 (gl:enable f)))))

(defun disable (&rest flags)
  (loop with h = (flags *state*)
        for f in flags
        do (multiple-value-bind (old found) (gethash f h)
             (if found
                 (progn
                   (when old
                     (notice-flag-changed *state* f))
                   (setf (gethash f h) nil))
                 (gl:disable f)))))


(defun maybe-call-draw-callback ()
  (when (draw-callback *state*)
    (let ((draws (3b-glim/s:get-draws))
          (buffers (3b-glim/s:get-buffers)))
      (when (and draws buffers)
        (funcall (draw-callback *state*)
                 (renderer-config *state*) draws buffers)))))


(defmethod begin-frame ((state glim-state))
  (3b-glim/s:reset-buffers *state*)
  (setf (state-changed-flags *state*) -1)
  (begin-frame (renderer-config state)))

(defmacro with-frame (() &body body)
  ` (prog1
        (with-balanced-matrix-stacks ()
          (begin-frame *state*)
          ,@body)
      (maybe-call-draw-callback)))

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

(defun set-primitive (primitive size)
  (setf (%primitive *state*) (list primitive size t)))


(defun restart-draw ()
  (let* ((state *state*)
         (primitive (car (primitive state))))
    (ecase primitive
      ((:triangles :points :lines :quads)
       ;; always draw whole primitive to buffer, so just reset index to 0
       (setf (primitive-index state) 0))
      (:line-strip
       ;; need to track 1 vertex from previous draw
       (setf (primitive-index state) 1))
      ((:triangle-strip :triangle-fan :quad-strip)
       ;; need to track 2 vertices from previous draw
       (setf (primitive-index state) 2)))))


(defun index-overflow ()
  (assert (primitive *state*))
  (3b-glim/s:end)
  (maybe-call-draw-callback)
  ;; possibly could skip index for :triangles primitive, but using it
  ;; anyway for consistency for now
  (3b-glim/s:begin :triangles :indexed t)
  (restart-draw))

(defun check-index-overflow ()
  ;; if we have too many verts for 16bit indices (+ some extra space
  ;; for safety), start a new batch
  (when (> (+ (3b-glim/s::next-index *state*) 6)
           65535)
    (index-overflow)
    t))

(defun %flag (offset value)
  (let ((pf (prim-flags *state*)))
    (declare (type octet-vector pf))
    (setf (aref pf offset) value)
    (3b-glim/s:attrib-u8v +flags+  pf)))

(defun begin (primitive)
  #++(declare (optimize speed))
  (let ((prim-size (gethash primitive *primitives*))
        (state *state*))
    (macrolet ((df (flag mode &rest more)
                 `(cond
                    ,@ (loop for (f m) on (list* flag mode more) by #'cddr
                             collect `((get-flag ,f)
                                       (unless (= (aref (draw-flags state)
                                                        +draw-flag-mode+)
                                                  (float ,m 0.0))
                                         (notice-state-changed
                                          state +state-changed-draw-flags+))
                                       (setf (aref (draw-flags state)
                                                   +draw-flag-mode+)
                                             (float ,m 0.0))))
                       (t
                        (unless (= (aref (draw-flags state)
                                         +draw-flag-mode+)
                                   (float +draw-mode-normal+ 0.0))
                          (notice-state-changed
                           state +state-changed-draw-flags+))
                        (setf (aref (draw-flags state)
                                    +draw-flag-mode+)
                              (float +draw-mode-normal+ 0.0)))))
               (dfb (flag mode &rest more)
                 `(cond
                    ,@ (loop for (f m) on (list* flag mode more) by #'cddr
                             collect  `((get-flag ,f)
                                        (setf (aref (draw-flags state)
                                                    +draw-flag-mode-back+)
                                              (float ,m 0.0))))
                       (t (setf (aref (draw-flags state)
                                      +draw-flag-mode-back+)
                                (float +draw-mode-normal+ 0.0))))))
      (ecase primitive
        (:points
         (%flag +prim-mode-flag+ +point-flag+)
         (df :point-smooth +draw-mode-smooth+))
        ((:lines :line-strip)
         (%flag +prim-mode-flag+ +line-flag+)
         (setf (line-temp state) (v4 0 0 0 1))
         (df :line-smooth +draw-mode-smooth+))
        ((:triangles :triangle-fan :triangle-strip)
         (%flag +prim-mode-flag+ +triangle-flag+)
         (df :filled-wireframe +draw-mode-filled-wireframe+
             :wireframe +draw-mode-wireframe+)
         (dfb :backface-fill +draw-mode-normal+
              :backface-filled-wireframe +draw-mode-filled-wireframe+
              :backface-wireframe +draw-mode-wireframe+
              :filled-wireframe +draw-mode-filled-wireframe+
              :wireframe +draw-mode-wireframe+))
        ((:quads :quad-strip)
         (%flag +prim-mode-flag+ +quad-flag+)
         (df :filled-wireframe +draw-mode-filled-wireframe+
             :wireframe +draw-mode-wireframe+)
         (dfb :backface-fill +draw-mode-normal+
              :backface-filled-wireframe +draw-mode-filled-wireframe+
              :backface-wireframe +draw-mode-wireframe+
              :filled-wireframe +draw-mode-filled-wireframe+
              :wireframe +draw-mode-wireframe+))))
    (setf (aref (draw-flags state) +draw-flag-lighting+)
          (if (get-flag :lighting) 1.0 0.0))
    ;; todo: error messages
    (assert prim-size)
    (assert (not (primitive state)))
    (setf (primitive-state state) 0)
    ;; fixme: don't reset these if they haven't changed, or don't
    ;; apply
    (flet ((f (f) (get-flag f))
           (fi (f) (if (get-flag f) 1 0)))
      (3b-glim/s:uniform 'mv (ensure-matrix :modelview))
      (3b-glim/s:uniform 'proj (ensure-matrix :projection))
      (when (get/clear-changed-flag state +state-changed-line-width+)
        (3b-glim/s:uniform 'line-width (current-line-width state)))
      (when (get/clear-changed-flag state +state-changed-point-size+)
        (3b-glim/s:uniform 'point-size (current-point-size state)))
      (when (get/clear-changed-flag state +state-changed-draw-flags+)
        (3b-glim/s:uniform 'draw-flags (draw-flags state)))
      (when (get/clear-changed-flag state +state-changed-tex+)
        (3b-glim/s:uniform :textures
                           (copy-seq (textures state)))
        (3b-glim/s:uniform 'tex-mode0 (cond
                                        ((get-flag :texture-3d) +tex-mode-3d+)
                                        ((get-flag :texture-2d) +tex-mode-2d+)
                                        ((get-flag :texture-1d) +tex-mode-1d+)
                                        (t +tex-mode-off+))))
      (when (get/clear-changed-flag state +state-changed-lighting+)
        (3b-glim/s:uniform 'lights-enabled
                           (if (f :lighting)
                               (vector (fi :light0) (fi :light1)
                                       (fi :light2) (fi :light3))
                               (vector 0 0 0 0)))
        (when (f :lighting)
          (3b-glim/s:uniform :lighting
                             (concatenate 'vector
                                          (aref (lights state) 0)
                                          (aref (lights state) 1)
                                          (aref (lights state) 2)
                                          (aref (lights state) 3))))))
    (3b-glim/s:begin :triangles :indexed t)
    (check-index-overflow)

    (set-primitive primitive prim-size)))

(defun end ()
  (assert (primitive *state*))
  (setf (line-temp *state*) nil)
  (3b-glim/s:end)
  #++(maybe-call-draw-callback)
  #++(finish-chunk))


(defun write-vertex-line (v)
  (declare (optimize speed)
           (type (float-vector 4) v))
  ;; todo: tess and non-smooth width 1 lines
  #++
  (or (use-tess *state*)
      (and (= (current-line-width *state*) 1)
           (not (gethash :line-smooth (flags *state*)))))


  ;; make sure we have space for a whole quad
  (check-index-overflow)

  ;; for line, we store start vertex in temp[0]
  (let* ((state *state*)
         (line-temp (line-temp state))
         (primitive-temp (primitive-temp state)))
    (declare (type (float-vector 4) line-temp)
             (type (simple-array cons (4))
                   primitive-temp))
    ;; if this is first vertex of a line, just store it
    (when (evenp (primitive-state state)) ;; 0 or 2
      (incf (the fixnum (primitive-state state)))
      (replace line-temp v)
      (3b-glim/s::copy-current-vertex-into state
                                           (aref primitive-temp 0))
      (return-from write-vertex-line nil))
    ;; otherwise, we are at enpoint of a segment, so add a quad to
    ;; buffers
    (let ((curr v)
          (prev line-temp)
          (line-width (current-line-width state))
          (i 0))
      (declare (type (float-vector 4) curr prev)
               (type single-float line-width)
               (type u16 i))
      ;; copy current vertex state
      (3b-glim/s::copy-current-vertex-into state
                                           (aref primitive-temp 1))
      ;; and restore state from start of line, so we can emit the
      ;; vertices
      (3b-glim/s::restore-vertex-copy state (aref primitive-temp 0))
      ;; store current point (in temp[0]) as tangent
      (3b-glim/s::attrib-f +tangent-width+
                           (aref curr 0) (aref curr 1) (aref curr 2)
                           line-width)
      ;; set corner and add points
      (%flag +corner-index-flag+ 0)
      (setf i (3b-glim/s::attrib-fv +position+ prev))
      (%flag +corner-index-flag+ 1)
      (3b-glim/s::attrib-fv +position+ prev)

      ;; restore state for end, and repeat
      (3b-glim/s::restore-vertex-copy state (aref primitive-temp 1))
      (3b-glim/s::attrib-f +tangent-width+
                           (aref prev 0) (aref prev 1) (aref prev 2)
                           line-width )
      ;; set corner and add points
      (%flag +corner-index-flag+ 2)
      (3b-glim/s::attrib-fv +position+ curr)
      (%flag +corner-index-flag+ 3)
      (3b-glim/s::attrib-fv +position+ curr)

      ;; emit indices
      (3b-glim/s:index (+ i 0) (+ i 1) (+ i 2))
      (3b-glim/s:index (+ i 0) (+ i 2) (+ i 3))

      ;; and update state
      (setf (primitive-state state) 2))))

(defun write-vertex-line-strip (v)
  ;; todo: tess and non-smooth width 1 lines

  ;; after first segment is done, we just update state to 'end of
  ;; segment'
  (when (>= (primitive-state *state*) 1)
    (rotatef (aref (primitive-temp *state*) 0)
             (aref (primitive-temp *state*) 1))
    (setf (primitive-state *state*) 1))
  ;; and let line code handle the rest
  (write-vertex-line v)
  (replace (line-temp *state*) v))

(defun write-vertex-point (v)
  (declare (optimize speed)
           (type (float-vector 4) v))
  ;; todo: tess path, non-smooth size=1

  ;; make sure we have space for a whole quad
  (check-index-overflow)
  (let* ((state *state*)
         (point-size (current-point-size state))
         (i 0))
    (declare (type single-float point-size)
             (type u16 i))
    ;; store width in 4th element of tangent
    (3b-glim/s::attrib-f +tangent-width+ 0.0 0.0 0.0 point-size)
    ;; write out vertex 4 times with different corners
    (%flag +corner-index-flag+ 0)
    (setf i (3b-glim/s::attrib-fv +position+ v))
    (%flag +corner-index-flag+ 1)
    (3b-glim/s::attrib-fv +position+ v)
    (%flag +corner-index-flag+ 2)
    (3b-glim/s::attrib-fv +position+ v)
    (%flag +corner-index-flag+ 3)
    (3b-glim/s::attrib-fv +position+ v)
    ;; and emit indices
    (3b-glim/s:index (+ i 0) (+ i 1) (+ i 2))
    (3b-glim/s:index (+ i 0) (+ i 2) (+ i 3))))

(defun write-vertex-quad (v)
  (declare (optimize speed)
           (type (float-vector 4) v))

  (let* ((c (primitive-state *state*))
         (c4 (mod c 4))
         (i 0))
    (declare (type u32 c c4)
             (type u16 i))
    (when (zerop c4)
      ;; make sure we have space for a whole quad
      (check-index-overflow))

    (%flag +corner-index-flag+ c4)
    (setf (primitive-state *state*) (mod (1+ c) 4))
    (setf i (3b-glim/s:attrib-fv +position+ v))
    (when (= c 3)
      (3b-glim/s:index (- i 3) (- i 2) (- i 1))
      (3b-glim/s:index (- i 3) (- i 1) (- i 0)))))

(defun write-vertex-quad-strip (v)
  (declare (optimize speed)
           (type (float-vector 4) v))

  ;; make sure we have space for a whole quad
  (check-index-overflow)

  (let* ((state *state*)
         (primitive-temp (primitive-temp state))
         (c (primitive-state state))
         (i 0))
    (declare (type u32 c)
             (type u16 i)
             (type (simple-array t (4)) primitive-temp))
    (when (= c 4)
      ;; when starting a quad after first, duplicate previous 2 vertices
      (3b-glim/s::copy-current-vertex-into state
                                           (aref primitive-temp 2))
      (3b-glim/s::restore-vertex-copy state
                                      (aref primitive-temp 1))
      (%flag +corner-index-flag+ 0)
      (3b-glim/s::emit-vertex state)
      (3b-glim/s::restore-vertex-copy state
                                      (aref primitive-temp 0))
      (%flag +corner-index-flag+ 1)
      (3b-glim/s::emit-vertex state)
      ;; restore current vertex
      (3b-glim/s::restore-vertex-copy state
                                      (aref primitive-temp 2))

      ;; and skip state to last 2 vertices
      (setf c 2))
    (%flag +corner-index-flag+ c)
    (setf i (3b-glim/s:attrib-fv +position+ v))
    (when (> c 1)
      ;; store vert for use in next quad
      (3b-glim/s::copy-current-vertex-into state
                                           (aref primitive-temp (- c 2))))
    ;; mod 8 so we can distinguish first quad from later quads
    (setf (primitive-state *state*) (mod (1+ c) 8))
    (when (= c 3)
      (3b-glim/s:index (- i 3) (- i 2) (- i 1))
      (3b-glim/s:index (- i 3) (- i 1) (- i 0)))))


(defun write-vertex (v)
  (declare (optimize speed)
           (type (float-vector 4) v))
  (let* ((state *state*)
         (primitive (car (primitive state)))
         (primitive-temp (primitive-temp state))
         (ps (primitive-state state)))
    (declare (type u16 ps)
             (type (simple-array t (4)) primitive-temp))
    (flet ((tri-corner (&optional (i ps))
             (declare (type u16 i))
             (%flag +corner-index-flag+ (mod i 3))
             nil))
      (ecase primitive
        ((:triangles)
         (tri-corner)
         (when (zerop ps)
           (check-index-overflow))
         (setf ps (mod (+ ps 1) 3))
         (setf (primitive-state state) ps)
         (3b-glim/s:index (3b-glim/s:attrib-fv +position+ v)))
        (:triangle-strip
         (tri-corner)
         (let ((p3 (mod ps 3)))
           (3b-glim/s::copy-current-vertex-into state
                                                (aref primitive-temp p3))
           ;; if we restarted a batch, we need to emit previous 2
           ;; vertices again, otherwise we share indices with previous
           ;; tris
           (when (and (check-index-overflow)
                      (> ps 2))
             (3b-glim/s::restore-vertex-copy
              state (aref primitive-temp (mod (+ p3 1) 3)))
             (3b-glim/s::emit-vertex state)
             (3b-glim/s::restore-vertex-copy
              state (aref primitive-temp (mod (+ p3 2) 3)))
             (3b-glim/s::emit-vertex state)
             (3b-glim/s::restore-vertex-copy
              state (aref primitive-temp p3)))

           ;; mod 6 so we can distinguish first tri
           (setf ps (mod (+ ps 1) 6))
           (setf (primitive-state state) ps)
           #++(3b-glim/s:index (3b-glim/s:attrib-fv +position+ v))
           (let ((i (3b-glim/s:attrib-fv +position+ v)))
             (declare (type u16 i))
             ;; index 0 and 1 don't make a triangle, otherwise emit
             ;; indices for a triangle
             (when (> i 1)
                                        ;(3b-glim/s:index (- i 2) (- i 1) i)
               (multiple-value-bind (f r) (floor i 2)
                 (3b-glim/s:index (* 2 (+ f (- r 1)))
                                  (1- (* f 2))
                                  i))))))
        (:triangle-fan
         (tri-corner)
         ;; if we restarted a batch, we need to emit previous 2
         ;; vertices again, otherwise we share indices with previous
         ;; tris
         (when (and (check-index-overflow)
                    (> ps 2))
           (3b-glim/s::restore-vertex-copy state (aref primitive-temp 0))
           (3b-glim/s::emit-vertex state)
           (3b-glim/s::restore-vertex-copy state (aref primitive-temp 1))
           (3b-glim/s::emit-vertex state)
           (3b-glim/s::restore-vertex-copy state (aref primitive-temp 2)))
         (let ((i (3b-glim/s:attrib-fv +position+ v)))
           (declare (type u16 i))
           (3b-glim/s::restore-vertex-copy state (aref primitive-temp ps))
           ;; index 0 and 1 don't make a triangle, otherwise emit
           ;; indices
           (when (> i 1)
             (3b-glim/s:index 0 (1- i) i))
           (setf (primitive-state state) (ecase ps (0 1) (1 2) (2 1)))))
        (:points
         (write-vertex-point v))
        ;; :lines and line-strips are converted to quads if size /= 1 or if
        ;;  smooth is on
        (:lines
         (write-vertex-line v))
        (:line-strip
         (write-vertex-line-strip v))
        (:line-loop
         ;; todo: needs to store state extra state across chunks to close
         ;; loop
         )
        ;; :quads and :quad-strip are converted to tris / tri-strips
        (:quads
         (write-vertex-quad v))
        (:quad-strip
         (write-vertex-quad-strip v))))))


(defun %vertex (x y z w)
  (declare (optimize speed) (type single-float x y z w))
  (let ((v (v4 x y z w)))
    (declare (type (float-vector 4) v)
             (dynamic-extent v))
    (write-vertex v)
    nil))

(declaim (inline vertex vertex-v))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (%vertex (f x) (f y) (f z) (f w)))

(defun vertex-v (v)
  (etypecase v
    ((float-vector 4)
     (%vertex (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
    ((float-vector 3)
     (%vertex (aref v 0) (aref v 1) (aref v 2) 1.0))
    ((float-vector 2)
     (%vertex (aref v 0) (aref v 1) 0.0 1.0))
    ((float-vector 1)
     (%vertex (aref v 0) 0.0 0.0 1.0))
    (vector
     (flet ((a (x &optional (d 0.0) )
              (if (array-in-bounds-p v x)
                  (aref v x)
                  d)))
       (vertex (a 0 0.0) (a 1 0.0) (a 2 0.0) (a 3 0.0))))
    (list (apply 'vertex v))))

(defun %normal (x y z)
  (declare (optimize speed) (type single-float x y z))
  (3b-glim/s:attrib-f +normal+ x y z))

(declaim (inline normal))
(defun normal (x y z)
  (%normal (f x) (f y) (f z)))

#++
(defun edge-flag (flag)
  (let ((s (gethash :flags (vertex-format *state*))))
    (when s
      (let ((v (current-vertex *state*)))
        (setf (aref v (+ (car s) 0)) (if flag 1 0))))))

(defun %multi-tex-coord (tex s tt r q)
  (declare (optimize speed) (type single-float s tt r q))
  (assert (or (eql tex 0)(eql tex :texture0)))
  (3b-glim/s:attrib-f +texture0+ s tt r q))

(declaim (inline multi-tex-coord tex-coord))
(defun multi-tex-coord (tex s &optional (tt 0.0) (r 0.0) (q 1.0))
  (%multi-tex-coord tex (f s) (f tt) (f r) (f q)))
(defun tex-coord (s &optional (tt 0.0) (r 0.0) (q 1.0))
  (%multi-tex-coord 0 (f s) (f tt) (f r) (f q)))

#++
(defun fog-coord (x))
(defun %color (r g b a)
  (declare (optimize speed) (type single-float r g b a))
  (3b-glim/s:attrib-u8sc +color+ r g b a))

(declaim (inline color))
(defun color (r g b &optional (a 1.0))
  (%color (f r) (f g) (f b) (f a)))


(defun %secondary-color (r g b)
  (declare (optimize speed) (type single-float r g b))
  (3b-glim/s:attrib-u8sc +secondary-color+ r g b))

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


#++
(defun map-draws (fun draws)
  (loop for draw in draws
        do (apply fun draw)))

(defun line-width (w)
  (notice-state-changed *state* +state-changed-line-width+)
  (setf (current-line-width *state*) (coerce w 'single-float)))

(defun point-size (w)
  (notice-state-changed *state* +state-changed-point-size+)
  (setf (current-point-size *state*) (coerce w 'single-float)))

(defun polygon-mode (face mode)
  (notice-state-changed *state* +state-changed-draw-flags+)
  (ecase face
    (:front
     (ecase mode
       ((:line :wireframe)
        (setf (gethash :wireframe (flags *state*)) t)
        (setf (gethash :filled-wireframe (flags *state*)) nil))
       (:fill
        (setf (gethash :wireframe (flags *state*)) nil)
        (setf (gethash :filled-wireframe (flags *state*)) nil))
       (:filled-wireframe
        (setf (gethash :wireframe (flags *state*)) nil)
        (setf (gethash :filled-wireframe (flags *state*)) t))
       #++
       (:point
        ;; todo ?
        )))
    (:back
     (ecase mode
       ((:line :wireframe)
        (setf (gethash :backface-wireframe (flags *state*)) t)
        (setf (gethash :backface-filled (flags *state*)) nil)
        (setf (gethash :backface-filled-wireframe (flags *state*)) nil))
       (:fill
        (setf (gethash :backface-wireframe (flags *state*)) nil)
        (setf (gethash :backface-filled (flags *state*)) t)
        (setf (gethash :backface-filled-wireframe (flags *state*)) nil))
       (:filled-wireframe
        (setf (gethash :backface-wireframe (flags *state*)) nil)
        (setf (gethash :backface-filled (flags *state*)) nil)
        (setf (gethash :backface-filled-wireframe (flags *state*)) t))))
    (:front-and-back
     (polygon-mode :front mode)
     (polygon-mode :back mode))))

(defun bind-texture (target name)
  (notice-state-changed *state* +state-changed-tex+)
  (setf (aref (textures *state*)
              (ecase target (:texture-1d 0) (:texture-2d 1) (:texture-3d 2)))
        name))

(defun light (light pname param)
  (assert (< -1 light (length (lights *state*))))
  (notice-state-changed *state* +state-changed-lighting+)
  (flet ((v3v () (map 'v3 'f param))
         (v4v () (map 'v4 'f param)))
    (ecase pname
      (:position
       ;; pos/dir are transformed to eye space by current mv matrix when
       ;; set
       (let* ((p (v4v))
              (v (sb-cga:vec (aref p 0) (aref p 1) (aref p 2)))
              (mv (ensure-matrix :modelview)))
         (if (zerop (aref p 3))
             (replace p (sb-cga:transform-direction v mv))
             (replace p (sb-cga:transform-point v mv)))
         (setf (light-position (aref (lights *state*) light))
               p)))
      (:ambient
       (setf (light-ambient (aref (lights *state*) light))
             (v4v)))
      (:diffuse
       (setf (light-diffuse (aref (lights *state*) light))
             (v4v)))
      (:specular
       (setf (light-specular (aref (lights *state*) light))
             (v4v)))
      (:spot-direction
       (let* ((p (v3v))
              (mv (ensure-matrix :modelview)))
         (setf (light-spot-dir (aref (lights *state*) light))
               (sb-cga:transform-direction p mv))))
      (:spot-exponent
       (setf (aref (light-spot-params (aref (lights *state*) light)) 0)
             (f param)))
      (:spot-cutoff
       (setf (aref (light-spot-params (aref (lights *state*) light)) 1)
             (f param)))
      (:constant-attenuation
       (setf (aref (light-attenuation (aref (lights *state*) light)) 0)
             (f param)))
      (:linear-attenuation
       (setf (aref (light-attenuation (aref (lights *state*) light)) 1)
             (f param)))
      (:quadratic-attenuation
       (setf (aref (light-attenuation (aref (lights *state*) light)) 2)
             (f param))))))
