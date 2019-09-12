(defpackage #:3b-glim
  (:use :cl)
  (:export
   ;; buffering/state tracking
   #:*state*
   #:with-state
   #:with-frame
   #:flush-output
   ;; geometry specification
   #:with-primitives #:with-primitive
   #:begin
   #:end
   #:vertex
   #:tex-coord
   #:normal
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


   ;; handles deprecated things, passes rest to GL
   #:enable
   #:disable
   ;; lighting etc
   #:shade-model
   #:light-model
   #:color-material

   ;; (possibly should import and reexport everything from GL, so can
   ;; just use this package with local gl: nickname?)

   #:fog-coord
   #:secondary-color
   #:get-draws
   #:map-draws
   #:perspective
   #:color
   #:frustum
   #:ortho
   #:look-at
   #:line-width
   #:point-size
   #:ensure-matrix
   #:viewport
   #:load-matrix))
