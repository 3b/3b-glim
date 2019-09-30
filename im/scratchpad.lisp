(defpackage #:3b-glim-scratchpad
  (:use :cl)
  (:local-nicknames (#:glim #:3b-glim))
  (:export
   #:display
   #:scratchpad
   #:run))
(in-package #:3b-glim-scratchpad)

(defclass scratchpad (glut:window)
  ((start :initform nil :accessor start))
  (:default-initargs :width 1024 :height 1024 :title "3b-glim scratchpad"
                     :mode '(:double :rgb :depth :multisample)
                     :right-menu '(:click :exit)))

(defmethod glut:display-window :before ((w scratchpad))
  (setf (start w)
        (coerce (/ (get-internal-real-time) internal-time-units-per-second)
                'double-float))
  (3b-glim/gl:init-state/gl)
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (glim:disable :dither)
  (glim:shade-model :smooth)
  (glim:light-model :light-model-local-viewer 1)
  (glim:color-material :front :ambient-and-diffuse)0
  (glim:enable :light0 :lighting :cull-face :depth-test))

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

(defmethod display (w now)
  )
(defmethod glut:display ((window scratchpad))
  (fps)
  (let ((now (- (/ (get-internal-real-time) internal-time-units-per-second)
                (start window))))
    (flet ((s (x)
             (* 0.1 (abs (sin (/ now x))))))
      (gl:clear-color (s 2) (s 3) (s 4) 1))
    (gl:clear :color-buffer :depth-buffer)
    (glim:with-frame ()
      (display window now))
    (glut:swap-buffers)))

(defmethod glut:reshape ((window scratchpad) width height)
  (gl:viewport 0 0 width height)
  (glim:matrix-mode :projection)
  (glim:load-identity)
  (glim:perspective 50 (/ width height) 0.5 20)
  (glim:matrix-mode :modelview)
  (glim:load-identity))


(defmethod glut::menu ((window scratchpad) menu id)
  (format t "~&got menu item ~s from menu ~s~%" menu id)
  (case id
    (:click
     (setf (glut::right-menu window)
           `(:click :exit)))
    (:exit
     (glut:destroy-current-window))))

(defmethod glut:keyboard ((w scratchpad) key x y))
(defmethod glut:keyboard :around ((window scratchpad) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc
     (glut:destroy-current-window))
    (t (when (next-method-p) (call-next-method)))))


(defvar *w* nil)
(defmethod glut:idle ((window scratchpad))
  (unless *w*
    (setf *w* window))
  (glut:post-redisplay))

(defun scratchpad (type &rest args)
  (3b-glim/gl:with-state/gl (:init nil)
    (glut:display-window (apply #'make-instance type args))))

#++
(ql:quickload '3b-glim/scratchpad)
#++
(scratchpad 'scratchpad :pos-x 2555)
#++
(glut:show-window)
#++
(glut:main-loop)
