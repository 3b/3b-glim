(in-package #:3b-glim/s)
#++(ql:quickload '3b-glim/s)

;; only supporting 32 attributes (most hardware is less)
(defconstant +max-attribute-index+ 32)

;; to optimize math, we assume no more than 512 bytes per vertex (most
;; hardware supports at most 2048 stride, a few support 4k, but max
;; attribs is usually 16, with a few up 32, and 32*VEC4 is 512 bytes,
;; so using that for now)
(defconstant +max-vertex-bytes+ 512)

;; for now only supporting 16bit indices
(deftype index-buffer-value () 'u16)
(deftype index-buffer () `(simple-array index-buffer-value (*)))

(defvar *state*)

(declaim (type (simple-array function (32)) *writers*))
;; storing current writers in a typed global to speed up ATTRIB*
;; functions a bit
(defvar *writers* (make-array +max-attribute-index+
                              :element-type 'function
                              :initial-element
                              (lambda (&rest r)
                                (declare (ignore r))
                                (error "bad attribute?"))))
#+sbcl
(declaim (sb-ext:always-bound *writers*))

(deftype vertex-offset () '(mod 512))

;; to simplify things, we always just allocate the max size for the
;; single-vertex temp space, since even if there is a single VBO per
;; attribute with 32 attributes, that is still only 16k
(deftype single-vertex-buffer () '(simple-array octet (512)))

(defclass attribute-format ()
  ((element-type :reader element-type :initarg :type)
   (offset :reader offset :initarg :offset)
   (attribute-index :reader attribute-index :initarg :attribute-index)
   (normalized :reader normalized :initarg :normalized :initform nil)
   (buffer-index :reader buffer-index :initarg :buffer-index)))

(defclass buffer-format ()
  ((stride :reader stride :initarg :stride)
   (attributes :reader attributes :initarg :attributes)))

(defclass vertex-format ()
  ((buffers :reader buffers :initarg :buffers)
   (attributes :reader attributes :initarg :attributes)))


;; safety 0 isn't consistently faster (and sometimes is slower), so
;; using safety 1 to catch bad input types rather than checking
;; manually
(defmacro define-writer-builders (scalar-name vector-name-format
                                  element-size declared-type accessor)
  (let ((names (cons
                scalar-name
                (loop for i from 2 to 4
                      collect (alexandria:format-symbol
                               *package* vector-name-format i)))))
    `(progn
       ,@
       (loop
         for i from 1
         for name in names
         collect
         `(defun ,name (buffer offset)
            (check-type buffer single-vertex-buffer)
            (assert (<= (+ offset ,(* i element-size)) +max-vertex-bytes+))
            (check-type offset vertex-offset)
            (let ((buffer buffer)
                  (offset offset))
              (declare (type single-vertex-buffer buffer)
                       (type (mod ,(- +max-vertex-bytes+ (* element-size i)))
                             offset))
              (lambda (x y z w)
                (declare (optimize (speed 3) (Safety 1))
                         (type ,declared-type x y z w)
                         (ignorable y z w)
                         (type single-vertex-buffer buffer)
                         (inline nibbles::ieee-single-set/le)
                         (type (mod ,(- +max-vertex-bytes+ (* i element-size)))
                               offset))
                ,@
                (loop
                  for j below i
                  for v in '(x y z w)
                  collect
                  `(setf (,accessor buffer (+ offset ,(* j element-size)))
                         ,v))
                nil)))))))


(define-writer-builders float-writer-builder "VEC~d-WRITER-BUILDER"
  4 single-float nibbles:ieee-single-ref/le)

(define-writer-builders u8-writer-builder "VEC~d/U8-WRITER-BUILDER"
  1 octet aref)
(define-writer-builders u16-writer-builder "VEC~d/U16-WRITER-BUILDER"
  2 u16 nibbles:ub16ref/le)
(define-writer-builders u32-writer-builder "VEC~d/U32-WRITER-BUILDER"
  4 u32 nibbles:ub32ref/le)

;; todo
#++
(define-writer-builders s8-writer-builder "VEC~d/S8-WRITER-BUILDER"
  1 s8 ???)
(define-writer-builders s16-writer-builder "VEC~d/S16-WRITER-BUILDER"
  2 s16 nibbles:sb16ref/le)
(define-writer-builders s32-writer-builder "VEC~d/S32-WRITER-BUILDER"
  4 s32 nibbles:sb32ref/le)



;;; todo: matrix attribs
(defparameter *attribute-types*
  (alexandria:plist-hash-table
   '(:float (float-writer-builder 1 4 :float)
     :vec2 (vec2-writer-builder 2 4 :float)
     :vec3 (vec3-writer-builder 3 4 :float)
     :vec4 (vec4-writer-builder 4 4 :float)
     :u8 (u8-writer-builder 1 1 :unsigned-byte)
     :u8vec2 (vec2/u8-writer-builder 2 1 :unsigned-byte)
     :u8vec3 (vec3/u8-writer-builder 3 1 :unsigned-byte)
     :u8vec4 (vec4/u8-writer-builder 4 1 :unsigned-byte)
     :u8vec4 (vec4/u8-writer-builder 4 1 :unsigned-byte)
     :u16 (u16-writer-builder 1 2 :unsigned-short)
     :u16vec2 (vec2/u16-writer-builder 2 2 :unsigned-short)
     :u16vec3 (vec3/u16-writer-builder 3 2 :unsigned-short)
     :u16vec4 (vec4/u16-writer-builder 4 2 :unsigned-short)
     :u32 (u32-writer-builder 1 4 :unsigned-int)
     :u32vec2 (vec2/u32-writer-builder 2 4 :unsigned-int)
     :u32vec3 (vec3/u32-writer-builder 3 4 :unsigned-int)
     :u32vec4 (vec4/u32-writer-builder 4 4 :unsigned-int)
     )))



;;; vertex format:
;; (buffer-count &rest attributes)
;;; ATTRIBUTES entries:
;; (index type &optional (buffer 0))
;;; INDEX should be small integers
;;  array will be allocated using them as index (current gl hardware
;;  supports up to 32 or so, so shouldn't be a problem for valid
;;  layouts)
;;; TYPE is :float, :vec3, :u8vec4 etc.
;;;  BUFFER is between 0 and (1- BUFFER-COUNT)

(defun normalize-and-validate-vertex-format (format)
  (destructuring-bind (buffer-count &rest attributes) format
    (assert (and (plusp buffer-count)
                 (<= buffer-count +max-attribute-index+)))
    (let ((atts (sort (copy-list attributes) '< :key 'car))
          (index-used (make-hash-table)))
      (setf atts
            (loop for (index .type .buffer) in atts
                  for type = (if (consp .type) (car .type) .type)
                  for normalize = (if (consp .type) (second .type) t)
                  for buffer = (or .buffer 0)
                  do (assert (and (>= index 0)
                                  (< index +max-attribute-index+)))
                     (assert (gethash type *attribute-types*))
                     (assert (and (>= buffer 0)
                                  (< buffer buffer-count)))
                     (assert (not (gethash index index-used)))
                     (setf (gethash index index-used) t)
                  collect (list index type buffer normalize)))
      (list* buffer-count atts))))

(defparameter *known-vertex-formats* (make-hash-table :test 'equal))
;;: compile from a shader (per API, so not here)
(defun align (x align)
  (* align (ceiling x align)))
(defun compile-buffer-format (format buffer)
  (let* ((offset 0)
         (stride nil)
         (align0 nil)
         (atts
           (loop for (index type attrib-buffer n) in (cdr format)
                 ;; fixme: do we need separate alignment
                 for (w c s) = (gethash type *attribute-types*)
                 for align = (max s 4)
                 when (= buffer attrib-buffer)
                   collect (make-instance 'attribute-format
                                          :offset (align offset align)
                                          :type type
                                          :attribute-index index
                                          :buffer-index attrib-buffer
                                          :normalized n)
                   and do (unless align0
                            (setf align0 align))
                          (setf offset (+ (align offset align)
                                          (* s c)))
                          (setf stride offset))))
    (when align0
      (setf stride (align stride align0))
      (make-instance 'buffer-format
                     :stride stride
                     :attributes atts))))

(defun compile-vertex-format (format)
  (when (typep format 'vertex-format)
    (return-from compile-vertex-format format))
  (let ((format (normalize-and-validate-vertex-format format)))
    (when (gethash format *known-vertex-formats*)
      (return-from compile-vertex-format
        (gethash format *known-vertex-formats*)))

    (let* ((buffers (loop for i below (car format)
                          when (compile-buffer-format format i)
                            collect it))
           (attributes (sort (copy-seq
                              (reduce 'append
                                      (mapcar 'attributes buffers)
                                      :from-end t))
                             '< :key 'attribute-index)))
      (setf (gethash format *known-vertex-formats*)
            (make-instance 'vertex-format
                           :buffers buffers
                           :attributes attributes)))))

(defun dump-vertex-format (f)
  (format t "~&~s attributes in ~s buffers:~%"
          (length (attributes f))
          (length (buffers f)))
  (loop for b in (buffers f)
        for i from 0
        do (format t " buffer ~s: stride ~s, ~s attributes~%"
                   i
                   (stride b)
                   (length (attributes b)))
           (loop for a in (attributes b)
                 for (nil c s) = (gethash (element-type a) *attribute-types*)
                 do (format t "   ~s: ~s @ ~s -> ~s~%" (index a)
                            (element-type a)
                            (offset a)
                            (+ (offset a) (* c s)))))
  f)

#++
(dump-vertex-format
 (compile-vertex-format
  '(2
    (0 :vec4)
    (1 :vec4)
    (3 :vec4)
    (4 :vec3)
    (5 :vec4)
    (7 :u8vec4)
    (6 :vec3))))


(defclass draw ()
  ((primitive :reader primitive :initarg :primitive)
   (shader :reader shader :initarg :shader)
   (vertex-base :reader vertex-base :initarg :vertex-base)
   (vertex-count :accessor vertex-count :initform 0)
   (index-base :reader index-base :initarg :index-base :initform nil)
   (index-count :accessor index-count :initform 0)
   (uniforms :Reader uniforms :initarg :uniforms)
   ;; # of elements needed to finish a primitive (fixme:possibly not used?)
   (overflow-threshold :reader overflow-threshold :initform 1
                       :initarg :overflow-threshold)
   ;; size/width if drawing matching primitive
   (point-size :reader %point-size :initarg :point-size)
   (line-width :reader %line-width :initarg :line-width)
   ;; list of states to gl:enable/gl:disable
   (enables :reader enables :initarg :enables)
   (disables :reader disables :initarg :disables)
   ;; scissor/viewport rect (x y wx wy) or nil
   (scissor :reader %scissor :initarg :scissor)
   (viewport :reader %viewport :initarg :viewport)))

(defclass writer-state ()
  ;; compiled vertex format
  ((vertex-format :reader vertex-format :initarg :format)
   ;; storage for 1 vertex, 1 per buffer in format
   (vertex-buffers :reader vertex-buffers :initarg :vertex-buffers)
   ;; storage for VERTEX-COUNT output vertices, contains 1
   ;; octet-buffer for each buffer in format
   (output-buffers :reader output-buffers :initarg :output-buffers
                   :accessor %output-buffers)
   ;; storage for (* 2 VERTEX-COUNT) indices
   (index-buffer :Reader index-buffer :initarg :index-buffer
                 :accessor %index-buffer)
   ;; functions to update vertex attribute N in VERTEX-BUFFERS
   (writers :reader writers :initarg :writers)
   ;; list of draws, first element is NIL if not currently in a draw
   (draws :accessor draws :initform (list nil))
   ;; size of OUTPUT-BUFFERS in vertices
   (vertex-count :reader vertex-count :initarg :vertex-count)
   ;; index of next vertex in OUTPUT-BUFFERS
   (buffer-vertex-index :accessor buffer-vertex-index :initform 0)
   ;; index of first vertex in OUTPUT-BUFFERS relative to total output
   (vertex-index-offset :accessor vertex-index-offset :initform 0)
   ;; offset of next free element in index-buffer
   (index-buffer-index :accessor index-buffer-index :initform 0)
   ;; index of first index in INDEX-BUFFER relative to total output
   (index-index-offset :accessor index-index-offset :initform 0)
   ;; filled-buffers list of (type count &rest buffers), where TYPE is
   ;; :index or :vertex, COUNT is # of elements in this buffer set,
   ;; and BUFFERS is one or more vectors of vertex/index data
   (filled-output-buffers :accessor filled-output-buffers :initform nil)
   ;; called when next primitive won't fit in buffer, by default
   ;; allocates new buffers and continues accumulating data. should
   ;; also have options to copy buffer to GPU immediately (possibly
   ;; copy in other thread?), or to copy and draw immediately (would
   ;; need to handle restarting loop/strip/fan primitives correctly
   ;; though)
   (overflow-callback :reader overflow-callback :initarg :overflow-callback)
   (uniforms :reader uniforms :initarg :uniforms :initform (make-hash-table))
   (uniform-delta :accessor uniform-delta :initform nil)
   ;; GL state that should be associated with subsequent draws, rather
   ;; than any that happen to be dispatched later
   (point-size :accessor current-point-size :initform 1.0)
   (line-width :accessor current-line-width :initform 1.0)
   ;; things to enable/disable on next draw, if different from value
   ;; in old-enables (so mixing with manual calls to
   ;; gl:enable/gl:disable will cause problems).
   (new-enables :reader new-enables :initform (make-hash-table))
   ;; todo: specify initial values (probably mostly match GL?)
   (old-enables :reader old-enables :initform (make-hash-table))
   ;; scissor/viewport rectangle (x y wx wy) or NIL
   (new-scissor :accessor new-scissor :initform nil)
   (old-scissor :accessor old-scissor :initform nil)
   (new-viewport :accessor new-viewport :initform nil)
   (old-viewport :accessor old-viewport :initform nil)))


(defmethod notice-flag-changed (state flag new))

(defun get-enable (flag)
  (multiple-value-bind (new found) (gethash flag (new-enables *state*))
    (if found
        (values new found)
        (gethash flag (old-enables *state*)))))

(defun enable (&rest flags)
  (loop with h = (new-enables *state*)
        for f in flags
        do (setf (gethash f h) t)
           (notice-flag-changed *state* f t)))

(defun disable (&rest flags)
  (loop with h = (new-enables *state*)
        for f in flags
        do (setf (gethash f h) nil)
           (notice-flag-changed *state* f nil)))

(defun collect-enables (state)
  (let ((enables nil)
        (disables nil))
    (loop with old = (old-enables state)
          for e being the hash-keys of (new-enables state)
            using (hash-value s)
          unless (eql s (gethash e old :new))
            do (if s
                   (push e enables)
                   (push e disables))
               (setf (gethash e old) s))
    (clrhash (new-enables state))
    (values enables disables)))

(defun collect-scissor-view (state)
  (let ((scissor (new-scissor state))
        (viewport (new-viewport state)))
    (if (equalp scissor (old-scissor state))
        (setf scissor nil)
        (setf (old-scissor state) scissor))
    (if (equalp viewport (old-viewport state))
        (setf viewport nil)
        (setf (old-viewport state) viewport))
    (values scissor viewport)))

(defun line-width (w)
  (setf (current-line-width *state*) (coerce w 'single-float)))

(defun point-size (w)
  (setf (current-point-size *state*) (coerce w 'single-float)))

(defun scissor (x y wx wy)
  (setf (new-scissor *state*) (list x y wx wy)))

(defun viewport (x y wx wy)
  (setf (new-viewport *state*) (list x y wx wy)))

(defmethod total-vertex-index ((state writer-state))
  (+ (vertex-index-offset state)
     (buffer-vertex-index state)))
(defmethod total-index-index ((state writer-state))
  (+ (index-index-offset state)
     (index-buffer-index state)))

(defun update-draw-size (state)
  (let ((draw (car (draws state))))
    (when (index-base draw)
      (setf (index-count draw)
            (- (total-index-index state)
               (index-base draw))))
    (setf (vertex-count draw)
          (- (total-vertex-index state)
             (vertex-base draw)))))

(defparameter *primitive-counts*
  (alexandria:plist-hash-table
   '(:point (1 1)
     :lines (2 2)
     :line-strip (2 1)
     :line-loop (2 1)
     :triangles (3 3)
     :triangle-fan (3 1)
     :triangle-strip (3 1))))

(defun primitive-size (primitive)
  (if (typep primitive '(cons (eql :patch)))
      (list (second primitive) (second primitive))
      (gethash primitive *primitive-counts*)))

(defun has-space-for-vertices (state count)
  (< (+ (buffer-vertex-index state) count)
     (vertex-count state)))

(defun has-space-for-indices (state count)
  (< (+ (index-buffer-index state) count)
     (length (index-buffer state))))

(defun start-draw (primitive state)
  (let ((psize (primitive-size primitive)))
    (unless (has-space-for-vertices state (car psize))
      (funcall (overflow-callback state) state :partial nil))
    (multiple-value-bind (enables disables)
        (collect-enables state)
      (multiple-value-bind (scissor viewport)
          (collect-scissor-view state)
        (push (make-instance 'draw
                             :vertex-base (total-vertex-index state)
                             :primitive primitive
                             :overflow-threshold (second psize)
                             :uniforms (alexandria:copy-hash-table
                                        (uniforms state))
                             :point-size (current-point-size state)
                             :line-width (current-line-width state)
                             :scissor scissor
                             :viewport viewport
                             :enables enables
                             :disables disables)
              (draws state))))))

(defun finish-draw (state)
  (update-draw-size state))

(defun allocate-output-buffers (format vertex-count)
  (loop for bf in (buffers format)
        collect (make-array (* vertex-count (stride bf))
                            :element-type 'octet
                            :initial-element 0)))

(defun allocate-index-buffer (vertex-count)
  (make-array (* vertex-count 2)
              :element-type 'u16
              :initial-element 0))

(defun overflow-new-buffers (state &key (partial nil))
  (when partial
    (update-draw-size state))
  (push (list* :vertex (buffer-vertex-index state) (output-buffers state))
        (filled-output-buffers state))
  (setf (%output-buffers state)
        (allocate-output-buffers (vertex-format state) (vertex-count state)))
  (incf (vertex-index-offset state) (buffer-vertex-index state))
  (setf (buffer-vertex-index state) 0))

(defun overflow-index-buffers (state)
  (push (list :index (index-buffer-index state) (index-buffer state))
        (filled-output-buffers state))
  (setf (%index-buffer state)
        (allocate-index-buffer (vertex-count state)))
  (incf (index-index-offset state) (index-buffer-index state))
  (setf (index-buffer-index state) 0))



(defun make-writer-state (format &key (vertex-count (expt 2 14))
                                   (overflow-callback 'overflow-new-buffer)
                                   (uniform-hash-test 'eql))
  ;; not sure if this should try to compile the format or not? might
  ;; be slow, so force user to do it manually so slow path is less
  ;; convenient?
  #++(setf format (compile-vertex-format format))
  (assert (< vertex-count (expt 2 30)))
  (assert (typep format 'vertex-format))
  (let ((vb (make-array (length (buffers format))))
        (writers (make-array +max-attribute-index+))
        (ob (allocate-output-buffers format vertex-count)))
    (loop for bf in (buffers format)
          for i from 0
          do (setf (aref vb i)
                   (make-array +max-vertex-bytes+ :element-type 'octet
                                                  :initial-element 0)))
    (loop for af in (attributes format)
          for buffer = (aref vb (buffer-index af))
          for offset = (offset af)
          for (writer) = (gethash (element-type af) *attribute-types*)
          do (setf (aref writers (attribute-index af))
                   (funcall writer buffer offset)))
    (make-instance 'writer-state
                   :overflow-callback overflow-callback
                   :vertex-count vertex-count
                   :writers writers
                   :output-buffers ob
                   :vertex-buffers (coerce vb 'list)
                   :index-buffer (allocate-index-buffer vertex-count)
                   :format format
                   :uniforms (make-hash-table :test uniform-hash-test))))

(defun uniform (name value)
  (let ((v (if (typep value 'sequence)
               (copy-seq value)
               value)))
    (setf (gethash name (uniforms *state*)) v)
    (when (uniform-delta *state*)
      (setf (gethash name (uniform-delta *state*)) v))))

(defun copy-current-vertex (state)
  ;; make a temp copy of current vertex state that can be restored later
  (loop for buffer in (buffers (vertex-format state))
        ;;for stride of-type vertex-offset = (stride buffer)
        for vb of-type octet-vector in (vertex-buffers state)
        collect (copy-seq vb)))

(defun copy-current-vertex-into (state copy)
  ;; copy current vertex state into a copy previously returned by
  ;; COPY-CURRENT-VERTEX (avoids allocation, and possibly copies fewer bytes)
  (loop for buffer in (buffers (vertex-format state))
        for vb of-type octet-vector in (vertex-buffers state)
        for stride of-type vertex-offset = (stride buffer)
        for ob of-type octet-vector in copy
        do (replace ob vb :end2 stride)))

(defun restore-vertex-copy (state copy)
  ;; overwrite current vertex state with previously state returned by
  ;; COPY-CURRENT-VERTEX
  (loop for buffer in (buffers (vertex-format state))
        for vb of-type octet-vector in (vertex-buffers state)
        for stride of-type vertex-offset = (stride buffer)
        for ob of-type octet-vector in copy
        do (replace vb ob :end2 stride)))

(declaim (inline next-index))
(defun next-index (state)
  ;; fixme: combine this with emit-vertex?
  (let* ((bvi (buffer-vertex-index state))
         (vio (vertex-index-offset state))
         (dvb (vertex-base (car (draws state)))))
    (declare (type (unsigned-byte 32) bvi)
             (type (unsigned-byte 32) vio dvb))
    (- (+ bvi vio) dvb)))

(defun emit-vertex (state)
  (declare (optimize speed)
           (type writer-state state))
  (unless (has-space-for-vertices state 1)
    (overflow-new-buffers state))
  ;; return index of vertex within draw, in case we are doing indexed
  ;; drawing
  (let* ((bvi (buffer-vertex-index state))
         (vio (vertex-index-offset state))
         (dvb (vertex-base (car (draws state))))
         (ret (- (+ bvi vio) dvb)))
    (declare (type (unsigned-byte 32) bvi)
             (type (unsigned-byte 32) vio dvb))
    (loop for buffer in (buffers (vertex-format state))
          for vb of-type octet-vector in (vertex-buffers state)
          for ob of-type octet-vector in (output-buffers state)
          for stride of-type vertex-offset = (stride buffer)
          for offset = (* stride bvi)
          do (replace ob vb :start1 offset :end2 stride))
    (setf (buffer-vertex-index state)
          (ldb (byte 32 0) (1+ bvi)))
    ret))

(declaim (inline attrib-f
                 attrib-u8 attrib-u16 attrib-u32 attrib-s16 attrib-s32
                 attrib-u8c attrib-u16c attrib-u32c attrib-s16c attrib-s32c
                 attrib-u8sc attrib-u16sc attrib-u32sc attrib-s16sc attrib-s32sc
                 %attrib0))

(defun %attrib0 (index)
  (when (zerop index)
    (emit-vertex *state*)))

(defun attrib-f (index x &optional (y 0.0) (z 0.0) (w 1.0))
  ;; converts int -> float to make it easier to use
  (funcall (aref *writers* index) (f x) (f y) (f z) (f w))
  (%attrib0 index))

;; these truncate, and error on out of range
(defun attrib-u8 (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u16 (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u32 (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-s16 (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-s32 (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))

;; these truncate, and clamp to valid range
(defun attrib-u8c (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max 0 (min 255 (truncate a)))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u16c (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max 0 (min 65535 (truncate a)))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u32c (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max 0 (min #xffffffff (truncate a)))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-s16c (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max #x-8000 #x7fff (truncate a))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-s32c (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max #x-80000000 #x7fffffff) (truncate a)))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))

;; these scale from 0.0-1.0 or -1.0..1.0 to range, truncate, and clamp
(defun attrib-u8sc (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a)
           (let ((a (max 0.0 (min 1.0 a))))
             (declare (type (single-float 0.0 1.0) a))
             (truncate (* a 255)))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u16sc (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max 0 (min 65535 (truncate (* a 65535))))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-u32sc (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max 0 (min #xffffffff (truncate (* a #xffffffff))))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
;; these scale and clamp to symmetrical +- (2^n - 1) range, so 0.0 =
;; 0. (matches GL normalized usage)
(defun attrib-s16sc (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max #x-7fff #x7fff (truncate (* a #x7fff)))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))
(defun attrib-s32sc (index x &optional (y 0) (z 0) (w 0))
  (flet ((a (a) (max #x-7fffffff #x7fffffff) (truncate (* a #x7fffffff))))
    (declare (inline a))
    (funcall (aref *writers* index) (a x) (a y) (a z) (a w)))
  (%attrib0 index))

;; todo: more of these
(declaim (inline attrib-u8v attrib-fv))

(defun attrib-fv (index v)
  (etypecase v
    ((simple-array single-float (4))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
    ((simple-array single-float (3))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) (aref v 2) 1.0))
    ((simple-array single-float (2))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) 0.0 1.0))
    (sequence
     (apply #'attrib-f (coerce v 'list))))
  (%attrib0 index))

(defun attrib-u8v (index v)
  (etypecase v
    ((simple-array octet (4))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
    ((simple-array octet (3))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) (aref v 2) 255))
    ((simple-array octet (2))
     (funcall (aref *writers* index)
              (aref v 0) (aref v 1) 0 255))
    (sequence
     (apply #'attrib-u8 (coerce v 'list))))
  (%attrib0 index))

(defun begin (primitive &key indexed shader)
  (let* ((state *state*)
         (uniform-delta (uniform-delta state)))
    (assert (not (car (draws state))))
    (multiple-value-bind (enables disables)
        (collect-enables state)
      (multiple-value-bind (scissor viewport)
          (collect-scissor-view state)
        ;; if this is first draw, or we changed shaders, update all
        ;; uniforms
        (when (or (not uniform-delta)
                  (not (eql shader
                            (shader (cadr (draws state))))))
          (setf uniform-delta (alexandria:copy-hash-table (uniforms state))))
        (setf (uniform-delta state) (make-hash-table))
        (setf (car (draws state))
              (make-instance 'draw :primitive primitive
                                   :shader shader
                                   :uniforms uniform-delta
                                   :vertex-base (total-vertex-index state)
                                   :index-base (when indexed
                                                 (total-index-index state))
                                   :point-size (current-point-size state)
                                   :line-width (current-line-width state)
                                   :scissor scissor
                                   :viewport viewport
                                   :enables enables
                                   :disables disables))))))

(defun end ()
  (update-draw-size *state*)
  (push nil (draws *state*)))

(defmacro with-state ((format) &body body)
  `(let* ((*state* (make-writer-state ,format))
          (*writers* (writers *state*)))
     ,@body))

(defmacro with-draw ((primitive &key shader indexed) &body body)
  `(unwind-protect
        (progn (begin ,primitive :indexed ,indexed :shader ,shader) ,@body)
     (end)))


(declaim (inline index))
(defun index (&rest indices)
  (declare (dynamic-extent indices))
  (let ((state *state*)
        (l (length indices)))
    (unless (has-space-for-indices state l)
      (overflow-index-buffers state))
    (let ((ib (index-buffer state)))
      (declare (type index-buffer ib))
      (loop for i of-type fixnum from (index-buffer-index state)
              below (length ib)
            for index in indices
            do (setf (aref ib i) index))
      (incf (the u32 (index-buffer-index state)) l))))

(defun reset-buffers (state)
  (setf (%output-buffers state)
        (allocate-output-buffers (vertex-format state) (vertex-count state)))
  (setf (vertex-index-offset state) 0)
  (setf (buffer-vertex-index state) 0)

  (setf (%index-buffer state) (allocate-index-buffer (vertex-count state)))
  (setf (index-index-offset state) 0)
  (setf (index-buffer-index state) 0)

  (setf (filled-output-buffers state) nil)

  (setf (uniform-delta state) nil)
  (setf (draws *state*) (list nil)))

(defun get-buffers ()
  (let ((state *state*))
    ;; if we have any partial buffers, flush them.
    ;; fixme: option to not allocate new ones if we are done?
    (unless (zerop (buffer-vertex-index state))
      (overflow-new-buffers state))
    (unless (zerop (index-buffer-index state))
      (overflow-index-buffers state))
    (setf (vertex-index-offset state) 0)
    (setf (index-index-offset state) 0)
    (loop with ib = nil
          with vb = (make-array (length (vertex-buffers state))
                                :initial-element nil)
          with isize = 0
          with sizes = (make-array (length (vertex-buffers state))
                                   :initial-element 0)
          for (type count . buffers) in (shiftf (filled-output-buffers state)
                                                nil)
          when (eql type :index)
            do (push (list (* count 2) (car buffers)) ib)
               (incf isize (* count 2))
          else do (loop for f in (buffers (vertex-format state))
                        for stride = (stride f)
                        for buf in buffers
                        for i from 0
                        do (incf (aref sizes i) (* stride count))
                           (push (list (* count stride) buf)
                                 (aref vb i)))
          finally (return (list :ibo-size isize :ibo-chunks ib
                                :vbo-sizes sizes :vbo-chunks vb)))))

(defun get-draws ()
  ;; inside a draw if car isn't NIL
  (assert (not (car (draws *state*))))
  (setf (uniform-delta *state*) nil)
  (nreverse (cdr (shiftf (draws *state*) (list nil)))))
