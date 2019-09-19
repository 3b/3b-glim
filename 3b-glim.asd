(defsystem 3b-glim
  :description "Immediate-mode 2d and 3d graphics library, intended for use with modern GL/GLES and Vulkan"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria sb-cga nibbles)
  :serial t
  :components ((:file "package")
               (:file "state")
               (:file "buffer")
               (:file "matrix-stack")
               (:file "2d")
               (:file "3d")
               ;; not sure if shaders will be 3bgl-shaders, or what?
               #++(:file "shaders")))

(defsystem 3b-glim/gl
  :description "OpenGL utilities for 3b-glim"
  :depends-on (cl-opengl 3b-glim 3bgl-shader)
  :serial t
  :components ((:file "shaders-gl")
               (:file "gl")))

;; not sure if this will be same as /gl or not?
#++
(defsystem 3b-glim/gles
  :description "OpenGL ES utilities for 3b-glim"
  :depends-on (cl-opengl/es2)
  :serial t
  :components ((:file "gles")))

;;todo
#++
(defsystem 3b-glim/vk
  :description "Vulkan utilities for 3b-glim"
  :serial t
  :components ((:file "vk")))

(defsystem 3b-glim/example
  :depends-on (3b-glim/gl cl-glut pngload)
  :serial t
  :components ((:file "example")))
