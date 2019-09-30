(defpackage #:3b-glim
  (:use :cl :3b-glim/common)
  (:import-from #:3b-glim/common
                #:*matrix-stacks*
                #:*matrices*
                #:*current-matrix*)
  (:export
   ;; buffering/state tracking
   #:*state*
   #:with-state
   #:with-frame
   #:flush-output

   #:get-draws
   #:map-draws

   ;; geometry specification
   #:with-draw
   #:begin
   #:end
   #:vertex
   #:tex-coord
   #:normal
   #:fog-coord
   #:secondary-color
   #:color

   #:index

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

   ;; handles deprecated things, passes rest to GL
   #:enable
   #:disable
   ;; lighting etc
   #:shade-model
   #:light
   #:light-model
   #:color-material
   ;; other state
   #:line-width
   #:point-size


   ;; symbols naming uniforms
   #:mv #:proj #:mvp #:normal-matrix
   #:tex-mode0
   #:tex0-1 #:tex0-2 #:tex0-3
   #:lights-enabled #:lights #:draw-flags
   ;; also
   ;;   #:line-width  #:point-size
   ;; from other groups

   #:polygon-mode
   #:bind-texture))
