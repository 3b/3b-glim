(defpackage #:3b-glim/s
  (:use :cl :3b-glim/common)
  (:export

   ;; buffering/state tracking
   #:*state*
   #:current-point-size
   #:current-line-width

   #:with-glim

   #:get-draws
   #:map-draws

   #:make-writer-state
   #:compile-vertex-format

   ;; matrix ops / utilities
   #:matrix-mode
   #:load-identity
   #:translate
   #:scale
   #:rotate
   #:mult-matrix
   #:mult-transpose-matrix
   #:with-pushed-matrix
   #:push-matrix
   #:pop-matrix
   #:ensure-matrix
   #:load-matrix
   #:copy-matrix

   #:look-at
   #:frustum
   #:ortho
   #:perspective

   ;; specify uniforms to save with future draws
   #:uniform

   ;; specify bounds of a draw
   #:with-primitives #:with-primitive
   #:begin
   #:end

   ;; general state
   #:enable
   #:disable
   #:line-width
   #:point-size

   ;; attribute specification within a draw
   #:attrib-f ;; coerces to floats if needed (todo: normalized ints?)
   ;; integer attributes, truncates if needed
   #:attrib-u8
   #:attrib-u16
   #:attrib-u32
   #:attrib-s16
   #:attrib-s32
   ;; truncates and clamps if needed
   #:attrib-u8c
   #:attrib-u16c
   #:attrib-u32c
   #:attrib-s16c
   #:attrib-s32c
   ;; scales from 0.0..1.0 or -1.0..1.0, truncates and clamps
   #:attrib-u8sc
   #:attrib-u16sc
   #:attrib-u32sc
   #:attrib-s16sc
   #:attrib-s32sc

   #:with-draw
   #:index
   #:with-state
   #:get-buffers
   #:reset-buffers
   #:copy-current-vertex
   #:attrib-u8v
   #:attrib-fv
   #:viewport
   #:scissor))
