(defpackage #:3b-glim
  (:use :cl)
  (:export
   ;; buffering/state tracking
   #:*state*
   #:with-state
   #:with-frame
   #:flush-output

   #:get-draws
   #:map-draws

   ;; geometry specification
   #:with-primitives #:with-primitive
   #:begin
   #:end
   #:vertex
   #:tex-coord
   #:normal
   #:fog-coord
   #:secondary-color
   #:color

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

   #:look-at
   #:frustum
   #:ortho
   #:perspective

   ;; handles deprecated things, passes rest to GL
   #:enable
   #:disable
   ;; lighting etc
   #:shade-model
   #:light-model
   #:color-material
   ;; other state
   #:line-width
   #:point-size

   ;;

))
