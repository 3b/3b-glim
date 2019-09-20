(defpackage #:3b-glim-example
  (:use :cl)
  (:local-nicknames (#:glim #:3b-glim)))
(in-package #:3b-glim-example)

(defclass 3b-glim-example (glut:window)
  ((shaders :accessor shaders :initform nil))
  (:default-initargs :width 1024 :height 1024 :title "3b-glim example"
                     :mode '(:double :rgb :depth :multisample)
                     :right-menu '(:click :exit)))

(defvar *primitives*
  #(:points :lines :line-strip :triangles :triangle-strip
    :triangle-fan :quads :quad-strip))
(defparameter *primitive* 0)
(defparameter *1-face* nil)
(defparameter *smooth* t)
(defparameter *low* nil)
(defparameter *back* nil)
(defparameter *tex* nil)
(defparameter *sphere* nil)
(defun solid-cube (&optional (x1 1))
  (declare (ignorable x1))
  (let ((v #((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)
             (-1 -1 -1) (1 -1 -1) (1 1 -1) (-1 1 -1))))
    (labels ((s (x)
               #++(/ (+ (mod (+ x1 (* x1 x)) 37)) 38.0)
               (/ x 6.0)
               #++(* 0.3 (+ 1 (sin (/ (get-internal-real-time)
                                      (* x internal-time-units-per-second))))))
             (q (a b c d)
               (glim:color (s 1) (s 2) (s 3))
               (glim:tex-coord 0 0)
               (apply 'glim:vertex (aref v a))
               (glim:color (s 2) (s 3) (s 4))
               (glim:tex-coord 1 0)
               (apply 'glim:vertex (aref v b))
               (glim:color (s 3) (s 4) (s 5))
               (glim:tex-coord 1 1)
               (apply 'glim:vertex (aref v c))
               (glim:color (s 4) (s 5) (s 6))
               (glim:tex-coord 0 1)
               (apply 'glim:vertex (aref v d))))
      (let ((w 16.0))
        (glim:point-size (+ 0 (random w)))
        (glim:line-width (+ 0 (random w)))
        #++(glim:line-width (expt 2 (- (log w 2) (random (log w 2)))))
        #++(glim:line-width (- w (log (random (expt 2.0 w)) 2)))
        #++(glim:line-width (- 16 (expt (random (expt 2.0 16)) 1/4))))
      ;;(glim:line-width 10)
      (if *smooth*
          (progn
            (glim:enable :line-smooth :point-smooth)
            (glim:polygon-mode :front-and-back :wireframe))
          (progn
            (glim:disable :line-smooth :point-smooth)
            (glim:polygon-mode :front-and-back :fill)))
      (when *back*
        (glim:polygon-mode :back :filled-wireframe))

      (glim:with-pushed-matrix (:modelview)
        (glim:scale 2 2 2)
        (glim:with-primitives (aref *primitives* *primitive*)
          (glim:normal 0 0 1)
          (q 0 1 2 3)
          (unless *1-face*
            (glim:normal 0 0 -1)
            (q 4 7 6 5)
            (glim:normal -1 0 0)
            (q 4 0 3 7)
            (glim:normal 1 0 0)
            (q 1 5 6 2)
            (glim:normal 0 1 0)
            (q 3 2 6 7)
            (glim:normal 0 -1 0)
            (q 4 5 1 0)))))))

(defun load-textures ()
  (format t "tex~%")
  (setf *tex* (gl:gen-textures 3))
  (gl:bind-texture :texture-1d (first *tex*))
  (gl:tex-image-1d :texture-1d 0 :rgba 256 0
                   :rgba :float
                   (coerce (mapcar (lambda (a) (coerce a 'single-float))
                                   (loop for i below 256
                                         collect (sin (/ i 256))
                                         collect (sin (/ i 128))
                                         collect (sin (/ i 64))
                                         collect 1))
                           '(simple-array single-float 1)))
  (gl:generate-mipmap :texture-1d)
  (gl:pixel-store :unpack-alignment 1)
  (gl:bind-texture :texture-2d (second *tex*))
  (pngload:with-png-in-static-vector (p "e:/tmp/crate1_diffuse.png" :decode t)
    (format t "2d ~s ~s ~s~%" (pngload:width p) (pngload:height p)
            (type-of (pngload:data p)))
    (gl:tex-image-2d :texture-2d 0 :rgb
                     (pngload:width p) (pngload:height p)
                     0
                     :rgba :unsigned-short
                     (static-vectors:static-vector-pointer (pngload:data p))))
  (gl:generate-mipmap :texture-2d)
  (gl:bind-texture :texture-3d (third *tex*))
  (gl:tex-image-3d :texture-3d 0 :rgb
                   64 64 64
                   0
                   :rgba :byte
                   (cffi:null-pointer))
  (gl:generate-mipmap :texture-3d))

(defmethod glut:display-window :before ((w 3b-glim-example))
  (3b-glim/gl:init-state/gl)
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (glim:disable :dither)
  (glim:shade-model :smooth)
  (glim:light-model :light-model-local-viewer 1)
  (glim:color-material :front :ambient-and-diffuse)
  (glim:enable :light0 :lighting :cull-face :depth-test)
  (load-textures)
  (setf (shaders w) (3b-glim/gl:load-shaders)))

(declaim (inline deg-to-rad rad-to-deg))
(defun deg-to-rad (x)
  (typecase x
    (single-float
     (float (* x (/ pi 180.0)) 1.9))
    (t (* x (/ pi 180)))))
(defun rad-to-deg (x)
  (typecase x
    (single-float
     (float (* x (/ 180.0 pi)) 1.0))
    (t (* x (/ 180 pi)))))

(defun perspective-matrix (fovy-degrees aspect z-near z-far)
  (let ((f (float (/ (tan (/ (deg-to-rad fovy-degrees) 2))) 1.0))
        (dz (- z-near z-far)))
    (sb-cga:matrix (/ f aspect) 0.0 0.0 0.0
                   0.0 f 0.0 0.0
                   0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
                   0.0 0.0 -1.0 0.0)))

(defvar *r* 0)
(defvar *frames* 0)
(defvar *frametime* nil)

(defun fps ()
  (unless *frametime*
    (setf *frametime* (get-internal-real-time))
    (setf *frames* 1))
  (incf *frames*)
  (let* ((n (get-internal-real-time))
         (s (float (/ (- n *frametime*)
                      internal-time-units-per-second))))
    (when (> s 5)
      (format t "~s frames in ~s sec = ~s fps (~s s)~%"
              *frames* s (/ *frames* s) (/ s *frames*))
      (setf *frametime* n)
      (setf *frames* 1))))
(defvar *rnd* (make-random-state))
(defvar *anim* t)
(defmethod glut:display ((window 3b-glim-example))
  (fps)
  (let ((now
          (if *anim*
              (* 0.1 (- (/ (get-internal-real-time)
                           internal-time-units-per-second)
                        13000))
              1230))
        (now2
          (* 0.6 (- (/ (get-internal-real-time)
                       internal-time-units-per-second)
                    23000))))
    (glim:with-frame ()
      (setf *r* glim::*state*)
      (glim:matrix-mode :modelview)
      (glim:load-identity)
      (glim:bind-texture :texture-1d 1)
      (glim:bind-texture :texture-2d 2)
      (glim:bind-texture :texture-3d 3)
      (glim:enable :texture-1d)
      (glim:enable :texture-2d)
      (glim:disable :texture-3d)
      (flet ((s (x)
               (* 0.1 (abs (sin (/ now x))))))
        (gl:clear-color (s 2) (s 3) (s 4) 1))
      (gl:enable :blend :depth-test
                 :polygon-smooth :sample-alpha-to-coverage)
      (gl:disable :cull-face :lighting :light0 :texture-2d
                  ;; :polygon-smooth
                  ;; :blend
                  ;; :sample-alpha-to-coverage
                  #++ :depth-test)
      (gl:clear :color-buffer :depth-buffer)

      (gl:blend-func :src-alpha :one-minus-src-alpha)
      ;;(gl:blend-func :src-alpha :one)
      #++(gl:blend-func :src-alpha :one)
      (gl:enable :multisample)
      #++(format t "~s ~s~%"(gl:get* :sample-buffers)
                 (gl:get* :samples))
      (glim:secondary-color (* 0.5 (+ 1 (sin now)))
                            (* 0.5 (+ 1 (sin (* 2 now))))
                            (* 0.5 (+ 1 (sin (* 3 now)))))
      (glim:with-pushed-matrix (:modelview)
        (glim:matrix-mode :modelview)
        #++(glim:look-at '(0 0 4) '(0 0 0) '(0 1 0))
        (glim:look-at '(0 0 4) '(0 0 0) '(0 1 0))
        (glim:light 0 :position (vector (* 8 (cos now2))
                                        0
                                        (* 8 (sin now2))
                                        1))
        (glim:light 0 :ambient '(0.1 0 0 1))
        (glim:light 0 :diffuse '(1 0 1 1))
        (glim:light 0 :specular '(0 0 1 1))

        (glim:light 1 :position (vector (sin now2) -1 (cos now2) 1))
        (glim:light 1 :ambient '(0 0 0 1))
        (glim:light 1 :diffuse '(0 11 0 1))
        (glim:light 1 :specular '(1 1 0 1))
        (glim:light 1 :constant-attenuation 0.0)
        (glim:light 1 :linear-attenuation 0.0)
        (glim:light 1 :quadratic-attenuation 9.0)
        (glim:enable :lighting :light0 :light1)

        (let ((*random-state* (make-random-state *rnd*)))
          (loop
            for i below (if *low* 100 1000)
            do (glim:with-pushed-matrix (:modelview)
                 (glim:matrix-mode :modelview)
                 (progn
                   (glim:scale 0.5 0.5 0.5)
                   (glim:rotate (* now (random 10.0))
                                0 1 0)
                   (glim:rotate (* now (random 10.0))
                                1 0 0)
                   (glim:translate (- (random 5.0) 2.5)
                                   (- (random 5.0) 2.5)
                                   (- (random 5.0) 2.5))
                   (glim:translate (sin (* 0.4 (+ now i)))
                                   (cos (/ (* 1 now) 1.2))
                                   (sin (/ now 1)))
                   (glim:rotate (* now 200)
                                0 0 1)
                   (glim:scale 0.08 0.08 0.08))
                 (solid-cube i)))
          (glim:color 1 1 1 1)
          (when *sphere*
            (glim:with-pushed-matrix (:modelview)
              (glim:matrix-mode :modelview)
              (glim:scale 0.75 0.75 0.75)
              (glim:rotate 60 -1 1 1)
              (glim:line-width 6)
              (glim:secondary-color 0.1 0.1 0.1)
              (glim:disable :texture-2d)
              (glim:disable :texture-1d)
              (glim:with-primitives :quads
                (flet ((v (x y z)
                         (let ((n (sb-cga:normalize (sb-cga:vec x y z))))
                           (glim:normal (aref n 0) (aref n 1) (aref n 2))
                           (glim:vertex x y z 1)))
                       (f (X) (coerce x 'single-float)))
                  (loop with s = 16
                        for i upto s
                        for i2 = (1+ i)
                        for x = (f (cos (* 2 i (/ pi s))))
                        for y = (f (sin (* 2 i (/ pi s))))
                        for x2 = (f (cos (* 2 i2 (/ pi s))))
                        for y2 = (f (sin (* 2 i2 (/ pi s))))
                        do (loop for j from 1 below s
                                 for j2 = (1+ j)
                                 for z = (f (cos (* j (/ pi s))))
                                 for z2 = (f (cos (* j2 (/ pi s))))
                                 for q = (f (sin (* j (/ pi s))))
                                 for q2 = (f (sin (* j2 (/ pi s))))
                                 ;;when (oddp j)
                                 do (progn
                                      (v (* q2 x) (* q2 y) z2)
                                      (v (* q2 x2) (* q2 y2) z2)
                                      (v (* q x2) (* q y2) z)
                                      (v (* q x) (* q y) z))))))))
          (glim:with-pushed-matrix (:modelview)
            (glim:matrix-mode :modelview)
            (glim:color 1 1 1 1)
            (glim:normal 0 1 0)
            (glim:with-primitives :quads
              (glim:vertex -10 -1.1 10)
              (glim:vertex 10 -1.1 10)
              (glim:vertex 10 -1.1 -10)
              (glim:vertex -10 -1.1 -10))))))
    (glut:swap-buffers)))

(defmethod glut:reshape ((window 3b-glim-example) width height)
  (gl:viewport 0 0 width height)
  (glim:matrix-mode :projection)
  (glim:load-identity)
  (glim:perspective 50 (/ width height) 0.5 20)
  (glim:matrix-mode :modelview)
  (glim:load-identity))


(defmethod glut::menu ((window 3b-glim-example) menu id)
  (format t "~&got menu item ~s from menu ~s~%" menu id)

  (case id
    (:click
     (setf (glut::right-menu window)
           `(:click :exit)))
    (:exit
     (glut:destroy-current-window))))

(defmethod glut:keyboard ((window 3b-glim-example) key x y)
  (declare (ignore x y))
  (case key
    (#\1 (setf *1-face* (not *1-face*)))
    (#\2 (setf *smooth* (not *smooth*))
     (format t "~&smooth = ~s~%" *smooth*))
    (#\3 (setf *back* (not *back*)))
    (#\4 (setf *low* (not *low*)))
    (#\p (setf *primitive* (mod (1+ *primitive*) (length *primitives*)))
     (format t "~&primitive = ~s~%"
             (aref *primitives* *primitive*)))
    (#\s (setf *sphere* (not *sphere*)))
    (#\space (setf *anim* (not *anim*)))
    (#\Esc
     (glut:destroy-current-window))))


(defvar *w* nil)
(defmethod glut:idle ((window 3b-glim-example))
  (unless *w*
    (setf *w* window)
    (format t "foo!~%"))
  (glut:post-redisplay))

(defun 3b-glim-example (&rest args)
  (3b-glim/gl:with-state/gl (:init nil)
    (glut:display-window (apply #'make-instance '3b-glim-example args))))

#++
(ql:quickload '3b-glim/example)
#++
(3b-glim-example :pos-x 2555)
#++
(glut:show-window)
#++
(glut:main-loop)
