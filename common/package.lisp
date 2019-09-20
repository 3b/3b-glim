(defpackage 3b-glim/common
  (:use :cl)
  (:export
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
   #:with-balanced-matrix-stacks

   #:look-at
   #:frustum
   #:ortho
   #:perspective

   ;; utilities/types
   #:octet #:u8 #:u16 #:u32
   #:octet-vector #:u16-vector #:u32-vector #:mat4x4
   #:f #:c4 #:v2 #:v3 #:v4

   ))
