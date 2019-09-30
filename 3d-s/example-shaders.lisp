(defpackage #:3b-glim-example/s-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vert1 #:frag1
           #:vert2 #:frag2
           #:vert3 #:frag3
           #:vert4 #:tess-con4 #:tess-eval4 #:frag4
           #:vert5 #:frag5
           #:vert6 #:frag6

           #:view #:modelview #:proj
           #:tex
           #:mouse

           #:pos #:normal #:uv #:color
           #:frag7
           #:vert7))
(in-package #:3b-glim-example/s-shaders)

(input pos :vec3 :location 0)
(input normal :vec3 :location 1)
(input uv :vec2 :location 2)
(input color :vec4 :location 3)

(uniform view :mat4)
(uniform modelview :mat4)
(uniform proj :mat4) ;; projection matrix
(uniform tex :sampler-2d)
(uniform mouse :ivec2)
(output color :vec4 :stage :fragment)

(interface footprint (:buffer (:fragment fp :vertex fp)
                      :layout (:binding 0 :std430 t))
  (uvc :vec2)
  (dudx :vec2)
  (dvdx :vec2))

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec3)
  (normal :vec3)
  (color :vec4)
  (uv :vec2))

(defun vert1 ()
  (let* ((mv-pos (* modelview (vec4 pos 1)))
         (nm (transpose (inverse (mat3 modelview)))))
    (setf gl-position (* proj mv-pos))
    (setf (@ outs normal) (* nm normal)
          (@ outs position) (vec3 (* (inverse view) modelview
                                     (vec4 pos 1)))
          (@ outs uv) (.xy uv)
          (@ outs color) color)))

(defun frag1 ()
  (let* ((s (texture tex (@ ins uv)))
         (l (* 1 (@ ins position)))
         (xy (- l (floor l) (vec3 0.5 0.0 0.5)))
         (w1 0.005)
         (fw (fwidth l))
         (ss (- (smooth-step (- (- w1) fw) (- fw w1) xy)
                (smooth-step (- w1 fw) (+ w1 fw) xy)))
         (s1 (+ 0
                (* (- 1 (.x ss))
                   (- 1 (.y ss))
                   (- 1 (.z ss))))))
    #++(setf color (* s (@ ins color)))
    (setf color (vec4  (mix (vec3 0 1 0)
                            (.xyz (@ ins color))
                            s1)
                       1))))

(defun frag2 ()
  (setf color (vec4 (vec3 (dot (normalize (vec3 1 1 1))
                               (normalize (@ ins normal))))
                    1)))

(defun frag3 ()
  (let* ((uv (* 10 (@ ins uv)))
         (dx (abs (dfdx uv)))
         (dy (abs (dfdy uv)))
         (w1 0.005)
         (w2 (* 5.5 w1))
         (xy (- uv (floor uv) 0.5))
         (r 1.6)
         (lx (* r (+ (.x dx) (.x dy))))
         (ly (* r (+ (.y dx) (.y dy))))
         (s1 (+ 0
                (* (- 1 (- (smooth-step (- (- w1) lx) (- lx w1) (.x xy))
                           (smooth-step (- w1 lx) (+ w1 lx) (.x xy))))
                   (- 1 (- (smooth-step (- (- w1) ly) (- ly w1) (.y xy))
                           (smooth-step (- w1 ly) (+ w1 ly) (.y xy)))))))
         (s2 (+ 0
                (* (- 1 (- (smooth-step (- (- w2) lx) (- lx w2) (.x xy))
                           (smooth-step (- w2 lx) (+ w2 lx) (.x xy))))
                   (- 1 (- (smooth-step (- (- w2) ly) (- ly w2) (.y xy))
                           (smooth-step (- w2 ly) (+ w2 ly) (.y xy))))))))
    (let ((c (.xyz (@ ins color)))
          (l1 (vec3 0 1 0))
          (l2 (vec3 0 0 0)))
      (setf color (vec4 (mix (mix l1 l2 s1) c s2) 1.0)))
    #++
    (when (= mouse (floor (.xy gl-frag-coord)))
      (setf (@ fp uvc) xy)
      (setf (@ fp dudx) dx)
      (setf (@ fp dvdx) dy)

      (setf color (vec4 1 0 1 1)))))




(defun frag4 ()
  (let ((s (texture tex (@ ins uv))))
    (setf color (* s (@ ins color)))))



(defun vert5 ()
  (let* ((mv-pos (* modelview (vec4 pos 1)))
         (nm (transpose (inverse (mat3 modelview)))))
    (setf gl-position (* proj mv-pos))
    (setf (@ outs normal) (* nm normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (+ (@ fp uvc) (* 0.5 (.xy uv)))
          (@ outs color) color)))

(defun frag5 ()
  #++
  (setf color (vec4 (* 10 (.xy (@ ins uv))) 0 1))
  (let ((s (- (smooth-step -0.025 -0.015 (@ ins uv))
              (smooth-step 0.015 0.025 (@ ins uv)))))
    (setf color (vec4 (mix (vec2 0) (vec2 1) s) 0 1))))



(defun vert6 ()
  (let* ((pos1 (+ pos (vec3 (* 0.5 uv (@ fp dudx)) 0)))
         (mv-pos (* modelview (vec4 pos1 1)))
         (nm (transpose (inverse (mat3 modelview)))))
    (setf gl-position (* proj mv-pos))
    (setf (@ outs normal) (* nm normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (.xy uv)
          (@ outs color) color)))

(defun frag6 ()
  (setf color (vec4 1 0 1 1)))

(defun vert7 ()
  (let* ((pos1 (+ pos (vec3 (* 0.5 uv (@ fp dvdx)) 0)))
         (mv-pos (* modelview (vec4 pos1 1)))
         (nm (transpose (inverse (mat3 modelview)))))
    (setf gl-position (* proj mv-pos))
    (setf (@ outs normal) (* nm normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (.xy uv)
          (@ outs color) color)))

(defun frag7 ()
  (setf color (vec4 0 1 1 1)))
