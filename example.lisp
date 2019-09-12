(defpackage #:3b-glim-example
  (:use :cl)
  (:local-nicknames (#:glim #:3b-glim)))
(in-package #:3b-glim-example)

(defclass 3b-glim-example (glut:window)
  ((shaders :accessor shaders :initform nil))
  (:default-initargs :width 1024 :height 1024 :title "3b-glim example"
                     :mode '(:double :rgb :depth)
                     :right-menu '(:click :exit)))


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
      (glim:with-primitives :quads
        (glim:normal 0 0 1)
        (q 0 1 2 3)
        (glim:normal 0 0 -1)
        (q 5 6 7 4)
        (glim:normal -1 0 0)
        (q 4 0 3 7)
        (glim:normal 1 0 0)
        (q 1 2 6 5)
        (glim:normal 0 1 0)
        (q 3 2 6 7)
        (glim:normal 0 -1 0)
        (q 4 0 1 5)))))

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
              (* 0.2 (- (/ (get-internal-real-time)
                           internal-time-units-per-second)
                        23000))
              1230)))
    (glim:with-frame ()
      (setf *r* glim::*state*)
      (glim:matrix-mode :modelview)
      (glim:load-identity)
      (flet ((s (x)
               (* 0.1 (abs (sin (/ now x))))))
        (gl:clear-color (s 2) (s 3) (s 4) 1))
      (gl:disable :cull-face :lighting :light0 :texture-2d)
      (gl:clear :color-buffer :depth-buffer)

      (glim:with-pushed-matrix (:modelview)
        (glim:matrix-mode :modelview)
        (glim:look-at '(0 0 4) '(0 0 0) '(0 1 0))
        (let ((*random-state* (make-random-state *rnd*)))
          (loop
            for i below 1000
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
                 (solid-cube i))))))
    (glut:swap-buffers)))

(defmethod glut:reshape ((window 3b-glim-example) width height)
  (glim:viewport 0 0 width height)
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

