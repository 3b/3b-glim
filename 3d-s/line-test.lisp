(defpackage  #:3b-glim-example/s-line-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:pos #:color #:flags #:tangent
           #:mv #:proj #:line-width #:dims
           #:fragment #:vertex))
(in-package #:3b-glim-example/s-line-shaders)

(input pos :vec3 :location 0)
(input tangent :vec3 :location 1)
(input color :vec4 :location 2)
(input flags :vec4 :location 3)


(uniform mv :mat4)   ;; model-view matrix
(uniform proj :mat4) ;; projection matrix
(uniform line-width :float)
(uniform dims :vec2) ;; viewport dimensions

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec3)
  (color :vec4))

(defun lines-bary (corner tx line-width dxy)
  (declare (out dxy)))


(defun vertex ()
  (let* ((mv-pos (* mv position))
         (corner (.x flags))
         (dxy (vec2 0 0))
         (pp (* proj mv-pos))
         (mv-p2 (* mv (vec4 (.xyz tangent) 1)))
         (pp2 (* proj mv-p2))
         (tn (- (/ (.xyz pp2) (.w pp2))
                (/ (.xyz pp) (.w pp))))
         (tx (normalize (.xy (cross tn (.xyz mv-pos)))))
         (s2 line-width)
         (s1 (- s2)))
    (case corner
      (1
       (setf tx (- tx))))
    (setf dxy (normalize (.xy tx)))
    (setf dxy (* s2 dxy))
    (when (< (abs (length dxy)) 2)
      (setf dxy (* 2 (normalize dxy))))
    (setf dxy (* (.xy dims) dxy))
    (setf dxy (* dxy (.w pp)))
    (setf gl-position (vec4 (+ (.xy pp) dxy) (.zw pp)))
    (setf (@ outs position) (vec3 mv-pos)
          (@ outs color) color)))

(output color :vec4 :stage :fragment)


(defun fragment ()
  (setf color (vec4 (.xyz (@ ins color)) 1)))


(defpackage #:3b-glim-example/s-line
  (:use :cl #:3b-glim-example/s #:3b-glim-example/s-line-shaders)
  (:local-nicknames (#:glim #:3b-glim/s)))
(in-package #:3b-glim-example/s-line)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec3)      ;; pos
     (1 :vec3)      ;; tangent
     (2 :u8vec4)    ;; color
     (3 :u8vec4)))) ;; flags

(defclass lines (scratchpad)
  ()
  (:default-initargs
   :shaders '((:line :vertex vertex :fragment fragment))))

(defvar *rnd* (make-random-state))
(defvar *anim* t)
(defvar *spin* t)
(defvar *low* nil)

(defun line (x1 y1 z1 x2 y2 z2 r g b)
  (let (i1 i2 i3 i4)
    (glim:attrib-u8sc 2 r g b 1)
    ;; submit first point twice, corners 0,1
    (glim:attrib-f 1 x2 y2 z2) ;; store opposite vertex to calculate tangent
    (glim:attrib-u8sc 3 0 0 0 0)
    (setf i1 (glim:attrib-f 0 x1 y1 z1))

    (glim:attrib-u8sc 3 1 0 0 0)
    (setf i2 (glim:attrib-f 0 x1 y1 z1))
    ;; and 2nd point twice, corners 0,1
    (glim:attrib-f 1 x1 y1 z1)
    (glim:attrib-u8sc 3 0 0 0 0)
    (setf i3 (glim:attrib-f 0 x2 y2 z2))

    (glim:attrib-u8sc 3 1 0 0 0)
    (setf i4 (glim:attrib-f 0 x2 y2 z2))

    (glim:index i1 i2 i3) ;; draw as 2 tris
    (glim:index i1 i3 i4)))

(defvar *r*)
(defmethod display ((w lines) now1)
  (let ((now (if *anim* now1 123.45)))
    (glim:with-state (*format*)
      (setf *r* glim::*state*)

      (glim:matrix-mode :modelview)
      (glim:load-identity)

      (gl:enable :blend :depth-test
                 :polygon-smooth :sample-alpha-to-coverage)
      (gl:disable :cull-face :lighting :light0 :texture-2d)

      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:enable :multisample)
      (glim:uniform 'proj (glim:copy-matrix :projection))
      (glim:uniform 'dims (vector (/ (wx w)) (/ (wy w))))

      (glim:with-pushed-matrix (:modelview)
        (glim:matrix-mode :modelview)
        (glim:look-at '(0 0 14) '(0 0 0) '(0 1 0))
        (glim:rotate (* 1 (if *spin* (* 6 now1)0) ) 0 1 0)
        (glim:uniform 'view (glim:copy-matrix))

        (let ((*random-state* (make-random-state *rnd*)))
          (flet ((r () (- (random 10.0) 5.0))
                 (c () (random 1.0)))
            (glim:uniform 'mv (glim:copy-matrix))
            (glim:uniform 'line-width 10 ;(* 5 (1+ (sin now)))
                          )
            (glim:with-draw (:triangles :shader :line :indexed t)
              (loop
                for i below (if *low* 100 2000)
                do (line (r) (r) (r)
                         (r) (r) (r)
                         (c) (c) (c)))))))

      (glim:with-pushed-matrix (:modelview)
        (glim:matrix-mode :modelview)
        (glim:look-at '(0 0 14) '(0 0 0) '(0 1 0))
        (glim:rotate (* 1 (if *spin* (* 6 now1) 90) ) 0 1 0)

                                        ;(glim:scale 0.1 0.1 0.1)
        (glim:uniform 'mv (glim:copy-matrix))
        (glim:uniform 'line-width 3)
        (glim:with-draw (:triangles :shader :line :indexed t)
          (loop with s1 = (+ 3 (sin (/ now 100)))
                with s2 = (+ 4 (sin (/ now 30)) (sin (/ now 120)))
                with s3 = (+ 4 (sin (/ now 50)))
                for i below 14000
                for a = (/ i (+ s1 72.0
                                (* 4 (sin (/ now 20)))))
                for b = (/ a (+ s2 2))
                for c = (/ a (+ s3 2))
                for x2 = (* 4 (sin a)) then x1
                for y2 = (* 4 (cos b)) then y1
                for z2 = (* 4 (cos c)) then z1
                for x1 = (* 4 (sin a))
                for y1 = (* 4 (cos b))
                for z1 = (* 4 (cos c))
                for t1 = (sb-cga:normalize
                          (sb-cga:vec (- x2 x1) (- y2 y1) (- z2 z1)))
                for o1 = (sb-cga:vec*
                          (sb-cga:normalize
                           (sb-cga:transform-point
                            (sb-cga:cross-product t1 (sb-cga:vec 1.0 0.0 0.0))
                            (sb-cga:rotate-around t1 (float (+ (* now 1.9)
                                                               (* i 0.6))
                                                            1.0))))
                          0.2)
                for r1 = (sb-cga:vec*
                          (sb-cga:normalize
                           (sb-cga:cross-product
                            o1 t1))
                          0.1)
                for tx = (aref r1 0)
                for ty = (aref r1 1)
                for tz = (aref r1 2)
                for ox = (aref o1 0)
                for oy = (aref o1 1)
                for oz = (aref o1 2)
                do (line (+ x1 ox ) (+ y1 oy) (+ z1 oz)
                         (+ x1 ox tx) (+ y1 oy ty) (+ z1 oz tz)
                         (abs (sin (/ i 400)))
                         (abs (cos (/ i 400)))
                         (* 0.5 (+ 1 (sin (* 0.04 i))))))))
      (dispatch-draws w))))



(defmethod keyboard ((w lines) key x y)
  (format t "k ~s~%" key)
  (case key
    (#\1 (setf *low* (not *low*)))
    (#\space (setf *anim* (not *anim*)))))
#++
(ql:quickload '3b-glim/example/s)
#++
(run 'lines :pos-x 2555)
#++
(glut:show-window)
#++
(glut:main-loop)
#++
(glut:leave-main-loop)
