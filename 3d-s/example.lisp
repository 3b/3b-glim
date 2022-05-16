(in-package #:3b-glim-example/s)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec3)      ;; pos
     (1 :vec3)      ;; normal
     (2 :vec2)      ;; texcoord
     (3 :u8vec4)))) ;; color

(defclass example (scratchpad)
  ((ssbo :initform nil :accessor ssbo))
  (:default-initargs
   :shaders '((:1 :vertex vert1 :fragment frag1)
              (:2 :vertex vert1 :fragment frag2)
              (:3 :vertex vert1 :fragment frag3)
              #++(:4 :vertex vert1 :tess-control tess-con4
                     :tess-eval tess-eval4 :fragment frag4))))

(defvar *rnd* (make-random-state))
(defvar *anim* t)
(defvar *spin* t)
(defvar *low* nil)

(defun solid-cube (&key (x1 1) (shader :1))
  (declare (ignorable x1))
  (let ((v #((-1 -1 1) (1 -1 1) (1 1 1) (-1 1 1)
             (-1 -1 -1) (1 -1 -1) (1 1 -1) (-1 1 -1))))
    ;; attribs 0=pos,1=normal,2=texcoord,3=color
    (labels ((s (x)
               #++(/ (+ (mod (+ x1 (* x1 x)) 37)) 38.0)
               (/ x 6.0)
               #++(* 0.3 (+ 1 (sin (/ (get-internal-real-time)
                                      (* x internal-time-units-per-second))))))
             (q (a b c d)
               (let (i1 i2 i3 i4)
                 (glim:attrib-u8sc 3 (s 1) (s 2) (s 3))
                 (glim:attrib-f 2 0 0)
                 (setf i1 (apply 'glim:attrib-f 0 (aref v a)))

                 (glim:attrib-u8sc 3 (s 2) (s 3) (s 4))
                 (glim:attrib-f 2 1 0)
                 (setf i2 (apply 'glim:attrib-f 0 (aref v b)))

                 (glim:attrib-u8sc 3 (s 3) (s 4) (s 5))
                 (glim:attrib-f 2 1 1)
                 (setf i3 (apply 'glim:attrib-f 0 (aref v c)))

                 (glim:attrib-u8sc 3 (s 4) (s 5) (s 6))
                 (glim:attrib-f 2 0 1)
                 (setf i4 (apply 'glim:attrib-f 0 (aref v d)))

                 (glim:index i1 i2 i3)
                 (glim:index i1 i3 i4))))
      (glim:with-pushed-matrix (:modelview)
        (glim:uniform 'modelview (glim:copy-matrix))
        (glim:with-draw (:triangles :indexed t :shader shader)
          (glim:attrib-f 1 0 0 1) ;; normal
          (q 0 1 2 3)
          (progn
            (glim:attrib-f 1 0 0 -1)
            (q 4 7 6 5)
            (glim:attrib-f 1 -1 0 0)
            (q 4 0 3 7)
            (glim:attrib-f 1 1 0 0)
            (q 1 5 6 2)
            (glim:attrib-f 1 0 1 0)
            (q 3 2 6 7)
            (glim:attrib-f 1 0 -1 0)
            (q 4 5 1 0)))))))

#++(defun load-textures ()
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

(defun curves ()
  (glim:with-pushed-matrix (:modelview)
    (glim:matrix-mode :modelview)
    (glim:load-identity)
    (glim:uniform 'modelview (glim:copy-matrix))
    (glim:attrib-u8sc 3 1 1 1 1)
    (glim:attrib-f 1 1 1 1)
    (glim:attrib-u8sc 3 1 0 1 1)
    (glim:with-draw (:line-strip :shader :1)
      (flet ((f (a) (coerce a 'single-float)))
        (loop for i from -1 to 1.00 by 0.025
              for s = (abs i)
              for a = (asin s)
              for c = (cos a)
              for f = (if (plusp i)
                          (/ (+ pi (* 2 a))
                             (* 2 pi))
                          (/ (- pi (* 2 a))
                             (* 2 pi)))
              for area = (+ (* f 2 pi) (*  (- i) c))
              for % = (f (/ area (* 2 pi)))
              do
                 (glim:attrib-f 0 (* 0.2 i) (* 0.2 %) -1 1))))
    (glim:attrib-u8sc 3 1 1 0)
    (glim:with-draw (:line-strip :shader :1)
      (loop for i from -1 to 1.00 by 0.025
            for tt = (max 0 (min 1 (* 0.5 (+ 1 i))))
            for ss = (* tt tt (- 3 (* tt 2)))
            do (glim:attrib-f 0 (* 0.2 i)
                              (* 0.2 ss)
                              -1 1)))
    (glim:attrib-u8sc 3 0 0 1)
    (glim:with-draw (:line-strip :shader :1)
      (loop for i from -1 to 1.00 by 0.025
            for tt = (max 0 (min 1 (* 0.5 (+ 1 i))))
            for ss = (* tt tt (- 3 (* tt 2)))
            do (glim:attrib-f 0 (* 0.2 i)
                              (* 0.2 (+ tt (* 1.8 (- tt ss))))
                              -1 1)))

    (glim:attrib-u8sc 3 0 1 1)
    (glim:with-draw (:line-strip :shader :1)
      (loop for i from -1 to 1.00 by 0.025
            for a = (asin i)
            for % = (/ (+ (+ pi (* 2 a))
                          (* (- i) (cos a)))
                       (* 2 pi))
            do
               (glim:attrib-f 0 (* 0.2 i)
                              (* 0.2 %)
                              -0.991 1)))))

(defun footprint ()
  (glim:line-width 2)
  (glim:with-pushed-matrix (:modelview)
    (glim:matrix-mode :modelview)
    (glim:load-identity)
    (glim:scale 0.2 0.2 0.8)
    (glim:uniform 'modelview (glim:copy-matrix))
    (glim:attrib-u8sc 3 1 1 1 1)
    (glim:attrib-f 1 1 1 1)
    (glim:with-draw (:triangle-strip :shader :5)
      (glim:attrib-f 2 -1 -1)
      (glim:attrib-f 0 -1 -1 -1)
      (glim:attrib-f 2 -1 1)
      (glim:attrib-f 0 -1 1 -1)
      (glim:attrib-f 2 1 -1)
      (glim:attrib-f 0 1 -1 -1)
      (glim:attrib-f 2 1 1)
      (glim:attrib-f 0 1 1 -1))
    (glim:translate 0 0 0.0001)
    (glim:uniform 'modelview (glim:copy-matrix))
    (glim:with-draw (:line-loop :shader :6)
      (glim:attrib-f 2 -10 -10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 -10 10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 10 10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 10 -10)
      (glim:attrib-f 0 0 0 -1))
    (glim:translate 0 0 0.0001)
    (glim:uniform 'modelview (glim:copy-matrix))
    (glim:with-draw (:line-loop :shader :7)
      (glim:attrib-f 2 -10 -10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 -10 10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 10 10)
      (glim:attrib-f 0 0 0 -1)
      (glim:attrib-f 2 10 -10)
      (glim:attrib-f 0 0 0 -1))))

(defmethod display ((w example) now1)
  (let ((now (if *anim* now1 123.45)))
    (glim:with-state (*format*)
      (setf *r* glim::*state*)
      (unless (ssbo w)
        (setf (ssbo w) (gl:gen-buffer))
        (gl:bind-buffer :shader-storage-buffer (ssbo w))
        (cffi:with-pointer-to-vector-data (p (make-array
                                              (* 4 128)
                                              :initial-element 0
                                              :element-type '(unsigned-byte 8)))
          (%gl:buffer-data :shader-storage-buffer (* 4 128) p :static-copy)))
      (%gl:bind-buffer-base :shader-storage-buffer 0 (ssbo w))
      (glim:matrix-mode :modelview)
      (glim:load-identity)

      (glim:enable :blend :depth-test :multisample
                          :polygon-smooth :sample-alpha-to-coverage)
      (glim:disable :cull-face :lighting :light0 :texture-2d)

      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (glim:uniform 'proj (glim:copy-matrix :projection))
      (glim:with-pushed-matrix (:modelview)
        (glim:matrix-mode :modelview)
        (glim:look-at '(0 0 4) '(0 0 0) '(0 1 0))
        (glim:rotate (* 1 (if *spin* (* 6 now1) 290)) 0 1 0)
        (glim:uniform 'view (glim:copy-matrix))

        (let ((*random-state* (make-random-state *rnd*)))
          (loop
            for i below (if *low* 100 2000)
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
                 (solid-cube :X1 i)))
          #++
          (glim:with-pushed-matrix (:modelview)
            (glim:matrix-mode :modelview)
            (glim:scale 1.4 1.4 1.4)
            (glim:translate -3.2 -0.1 -3.2)
            (solid-cube)
            (glim:translate 0 0 4)
            (solid-cube)
            (glim:translate 8 0 0)
            (solid-cube))
          (glim:attrib-u8sc 3 1 1 1 1)
          (glim:with-pushed-matrix (:modelview)
            (glim:matrix-mode :modelview)
            (glim:scale 0.75 0.75 0.75)
            (glim:rotate 60 -1 1 1)
            (glim:uniform 'modelview (glim:copy-matrix))
            (glim:with-draw (:triangles :shader :2)
              (flet ((v (x y z)
                       (let ((n (sb-cga:normalize (sb-cga:vec x y z))))
                         (glim:attrib-f 1 (aref n 0) (aref n 1) (aref n 2))
                         (glim:attrib-f 0 x y z 1)))
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

                                    (v (* q2 x) (* q2 y) z2)
                                    (v (* q x2) (* q y2) z)
                                    (v (* q x) (* q y) z)))))))
          (glim:uniform 'mouse (vector (mx w) (- (wy w) (my w))))
          (glim:with-pushed-matrix (:modelview)
            (glim:matrix-mode :modelview)
            (glim:translate 0 3.5 -1)
            (glim:scale 5 5 5)
            (glim:uniform 'modelview (glim:copy-matrix))
            (glim:attrib-u8sc 3 1 1 1 1)
            (glim:attrib-f 1 0 1 0)
            (solid-cube :shader :3))

          #++
          (curves)

          #++
          (footprint)))
      (dispatch-draws w))))



(defmethod keyboard ((w example) key x y)
  (format t "k ~s~%" key)
  (case key
    (#\2 (reload-shader w :5 :vertex 'vert5 :fragment 'frag5)
     (reload-shader w :6 :vertex 'vert6 :fragment 'frag6)
     (reload-shader w :7 :vertex 'vert7 :fragment 'frag7))
    (#\1 (setf *low* (not *low*)))
    (#\space (setf *anim* (not *anim*)))))
#++
(ql:quickload '3b-glim/example/s)
#++
(run 'example :pos-x 2555)
#++
(glut:show-window)
#++
(glut:main-loop)
#++
(glut:leave-main-loop)
