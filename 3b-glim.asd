(defsystem 3b-glim/common
  :license "MIT"
  :depends-on (alexandria sb-cga)
  :serial t
  :pathname "common/"
  :components ((:file "package")
               (:file "utils")
               (:file "matrix-stack")))

(defsystem 3b-glim
  :description "Immediate-mode 2d and 3d graphics library, intended for use with modern GL/GLES and Vulkan"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (3b-glim/common 3b-glim/s alexandria)
  :serial t
  :pathname "im/"
  :components ((:file "package")
               (:file "state")))

(defsystem 3b-glim/gl
  :description "OpenGL utilities for 3b-glim"
  :depends-on (cl-opengl 3b-glim 3bgl-shader)
  :serial t
  :pathname "im/"
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
  :pathname "im"
  :components ((:file "example")))

(defsystem 3b-glim/scratchpad
  :depends-on (3b-glim/gl cl-glut pngload)
  :serial t
  :pathname "im"
  :components ((:file "scratchpad")))





(defsystem 3b-glim/s
  :description "Library for building vertex buffers for GL etc. using immediate-mode GL style "
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria nibbles 3b-glim/common)
  :serial t
  :pathname "3d-s/"
  :components ((:file "package")
               (:file "buffer")))


(defsystem 3b-glim/example/s
  :depends-on (3b-glim/s cl-glut pngload  3bgl-shader)
  :serial t
  :pathname "3d-s"
  :components ((:file "example-shaders")
               (:file "scratchpad")
               (:file "example")))
