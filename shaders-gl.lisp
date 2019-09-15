(defpackage #:3b-glim/gl-shaders
  (:use #:3bgl-glsl/cl)
  (:import-from #:3b-glim
                #:mv #:proj #:mvp #:normal-matrix
                #:tex-mode0 #:tex-mode1
                #:tex0-1 #:tex0-2 #:tex0-3
                #:tex1-1 #:tex1-2 #:tex1-3
                #:line-width #:point-size #:light-position
                #:draw-flags)
  (:export #:vertex
           #:fragment))
(in-package #:3b-glim/gl-shaders)

(input position :vec4 :location 0)
(input texture0 :vec4 :location 1)
(input texture1 :vec4 :location 2)
(input color :vec4 :location 3)
(input normal :vec3 :location 4)
(input tangent :vec4 :location 5)
(input secondary-color :vec3 :location 6)
(input flags :vec4 :location 7)

(uniform mv :mat4) ;; model-view matrix
;(uniform mvp :mat4) ;; model-view-projection matrix
(uniform proj :mat4) ;; projection matrix
(uniform normal-matrix :mat4)
(uniform tex-mode0 :int)
(uniform tex-mode1 :int)
(uniform tex0-1 :sampler-1d)
(uniform tex1-1 :sampler-1d)
(uniform tex0-2 :sampler-2d)
(uniform tex1-2 :sampler-2d)
(uniform tex0-3 :sampler-3d)
(uniform tex1-3 :sampler-3d)

(uniform line-width :float)
(uniform point-size :float)
(uniform draw-flags :int)
(uniform light-position :vec3)

(uniform dims :vec4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec3)
  (normal :vec3)
  (color :vec4)
  (color2 :vec3)
  (uv :vec3)
  (uv2 :vec3)
  (eye-direction :vec3)
  (light-direction :vec3)
  (bary :vec4)
  (flags :vec4 :flat))



(defun vertex ()
  (let* ((mv-pos (* mv position))
         (eye-dir (- (vec3 mv-pos)))
         (mode (.x flags))
         (dxy (vec4 0 0 0 0))
         (pp (* proj mv-pos))
         (bary (vec4 0)))
    (case mode
      (0 ;; tri
       ;; only need to set up barycentric coords for wireframe mode
       (case (.z flags)
         (0 (setf (.x bary) 1))
         (1 (setf (.y bary) 1))
         (2 (setf (.z bary) 1))
         (t (setf (.w bary) 1)))
       (setf (.w bary) line-width)
       ;; workaround for type inference bug
       0)
      (3 ;; quads
       ;; only need to set up barycentric coords for wireframe mode
       (case (.z flags)
         (0 (setf (.xy bary) (vec2 -1 1)))
         (1 (setf (.xy bary) (vec2 -1 -1)))
         (2 (setf (.xy bary) (vec2 1 -1)))
         (3 (setf (.xy bary) (vec2 1 1))))
       (let ((w 1))
         ;(setf (.xy bary) (.xy bary))
         (setf (.zw bary)  (vec2 line-width))
         #++(setf (.w bary) (* line-width (.w dims)))
         )
       ;; workaround for type inference bug
       0)
      (1
       ;; points
       (let* ((dist (length (vec3 mv-pos)))
              (corner (.z flags))
              (smooth1 (= 1 1))
              (s2 (+ point-size
                     (if smooth1 2 0)))
              (s1 (- s2)))
         (case corner
           (0
            (setf (.xyz bary) (vec3 s1 s1 point-size))
            (setf dxy (vec4 -1 -1 0 0)))
           (1
            (setf (.xyz bary) (vec3 s1 s2 point-size))
            (setf dxy (vec4 -1 1 0 0)))
           (2
            (setf (.xyz bary) (vec3 s2 s2 point-size))
            (setf dxy (vec4 1 1 0 0)))
           (3
            (setf (.xyz bary) (vec3 s2 s1 point-size))
            (setf dxy (vec4 1 -1 0 0)))
           (t
            (setf dxy (vec4 33 33 0 0))))
         (setf (.xy dxy) (* (.zw dims) s2 (.xy dxy)))
         (setf dxy (* dxy (.w pp))))
       0)
      (2 ;; line
       (let* ((dist (length (vec3 mv-pos)))
              (smooth1 (= 1 1))
              (corner (.z flags))
              (mv-p2 (* mv (vec4 (.xyz tangent) 1)))
              (pp2 (* proj mv-p2))
              (tn (- (/ (.xyz pp2) (.w pp2))
                     (/ (.xyz pp) (.w pp))))
              (tl (length (* (.xy tn) (/ (.zw dims)) 0.5)))
              (tx (normalize (.xy (cross tn (.xyz mv-pos)))))
              (lw line-width)
              (s2 (+ lw (if smooth1 2.0 0.0)))
              (s1 (- s2)))
         (if smooth1
             (let ((d (vec2 1 1))
                   (ty (vec2 (- (.y tx)) (.x tx))))
               (case corner
                 (0
                  (setf tx (- tx))
                  (setf bary (vec4 (- s1 tl) s1 lw tl))
                  (setf d (vec2 1 -1)))
                 (1
                  (setf tx (- tx))
                  (setf bary (vec4 (- s1 tl) s2 lw tl))
                  (setf d (vec2 1 1)))
                 (2
                  (setf bary (vec4 (+ s2 tl) s2 lw tl))
                  (setf d (vec2 1 1)))
                 (3
                  (setf bary (vec4 (+ s2 tl) s1 lw tl))
                  (setf d (vec2 1 -1))))
               (setf dxy (vec4 (+ (* (.y d) tx)
                                  ty
                                  0)
                               0 0)))
             (let ((d 1))
               (case corner
                 (0
                  (setf tx (- tx))
                  (setf d -1))
                 (1
                  (setf tx (- tx))
                  0)
                 (3
                  (setf d  -1)))
               (setf dxy (vec4 (* d (normalize (.xy tx)))
                               0 0))))
         (setf (.xy dxy) (* s2 (.xy dxy)))
         (when (< (abs (length dxy)) 2)
           (setf dxy (* 2 (normalize dxy))))
         (setf (.xy dxy)
               (* (.zw dims) (.xy dxy)))
         (setf dxy (* dxy (.w pp))))
       0)
      (t
       0))

    (setf gl-position (+ pp dxy))
    (setf (@ outs normal) (* (mat3 normal-matrix) normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (.xyz texture0)
          (@ outs uv2) (.xyz texture1)
          (@ outs color) color
          (@ outs color2) secondary-color
          ;; interpolated lighting parameters
          (@ outs light-direction) (- light-position (vec3 mv-pos))
          (@ outs eye-direction) eye-dir
          (@ outs flags) flags
          (@ outs bary) bary)))

(output color :vec4 :stage :fragment)

(defun smooth-point (bary)
  (let* ((ps (.z bary))
         (r (length (.xy bary)))
         (s (* #.(/ (sqrt 2.0) 2)
               (length (vec2 (dfdx r) (dfdy r)))))
         #++(s (fwidth r))
         (mps (max 1.5 ps))
         (e (smooth-step (+ mps s) (- mps s) r))
         #++(e (step r ps)))
    (when (<= e 0.0)
      (discard))
    (return (* e (min ps 1)))))

(defun smooth-line (bary)
  (let* ((ps (.z bary))
         (tl (* 1 (.w bary)))
         (x (abs (.x bary)))
         (y (abs (.y bary)))
         (s1 (* 1 #.(sqrt 2.0)
                (length (vec2 (dfdx y) (dfdy y)))))
         (r (length (vec2 (- x tl) y)))
         (s2 (* 1 #.(sqrt 2.0)
                (length (vec2 (dfdx r) (dfdy r)))))
         (mps (max ps 1.5))
         #++(s (fwidth r))
         (e (if (< x (* 1 tl))
                (smooth-step (+ mps s1) (- mps s1) y)
                (smooth-step (+ mps s2) (- mps s2) r)))
         #++(e (if (< x (* 1 tl))
                   (step y (max 1 ps))
                   (step r (max 1 ps)))))
    (when (<= e 0)
      (discard))
    (return (* e (min ps 1)))))

(defun wire-tri (bary)
  (let* ((fw (fwidth (.xyz bary)))
         (d (min (.x fw) (min (.y fw) (.z fw))))
         (w (.w bary))
         (mw (max 1.5 w))
         (a (smooth-step (- (* fw mw) d)
                         (+ (* fw mw) d)
                         (.xyz bary)))
         (m (- 1 (min (.x a) (min (.y a) (.z a))))))
    (when (<= m 0)
      (discard))
    (return (* m 1 (min w 1)))))

(defun wire-quad (bary)
  (let* ((d  (fwidth (.xy bary)))
         (dx (* 1 (abs (dfdx (.xy bary)))))
         (dy (* 1 (abs (dfdy (.xy bary)))))
         (w (.w bary))
         (w1 (- 1 (* (max 2 (.w bary))
                     (vec2 (max (.x dx) (.x dy))
                           (max (.y dx) (.y dy))))))
         #++(w2 (- 1 (* (max 1 (.w bary))
                      (vec2 (max (.x dx) (.x dy))
                            (max (.y dx) (.y dy))))
                (- d)))
         #++(a (step (vec2 (- 1 w))
                     (abs (.xy bary))))
         (a (smooth-step (- w1 d)
                         (+ w1 d)
                         (abs (.xy bary))))
         (m (max (.x a) (.y a))))
    (when (<= m 0 )
      (discard))
    (return (* m (min w 1)))))


(defun smoothing (mode bary)
  (let ((a 1.0))
    (case 2 ; draw-flags
      ;; normal
      (0 (setf a 1.0))
      ;; smooth
      (1 (case mode
           (0 ;; tri
            #++(setf a (max 0.1 (length bary)))
            (setf a 0.4))
           (3 ;; quad
            #++(setf a (wire-quad bary))
            (setf a 0.5))
           (1 ;; points
            (setf a (smooth-point bary)))
           (2 ;; lines
            (setf a (smooth-line bary)))
           ;;
           (t (setf a 1.0))))
      ;; wireframe
      (2
       (case mode
         (0 ;tri
          (setf a (wire-tri bary)))
         (3 ;quad
          (setf a (wire-quad bary)))))
      (t
       (setf a 1.0)))
    (return a)))
(defun fragment ()
  (let* ((normal (normalize (@ ins normal)))
         ;;(eye-direction (normalize (@ ins eye-direction)))
         ;;(light-direction (normalize (@ ins light-direction)))
;;;; calculate some intermediate values
         ;;   (l-dot-n (clamp (dot light-direction normal) 0 1))
         ;;   (r (reflect (- light-direction) normal))
         ;;   (r-dot-v (clamp (dot r eye-direction) 0 1))
         ;;   (distance (length (@ ins eye-direction)))
         (uv (@ ins uv))
         (t0 (case tex-mode0
               (1
                (texture tex0-1 (.x uv)))
               (2
                (texture tex0-2 (.xy uv)))
               (3
                (texture tex0-3 (.xyz uv)))
               (t
                (vec4 1 1 1 1))))
         (a (smoothing (.x (@ ins flags)) (@ ins bary))))
    (declare (:float a))
    #++(setf color  (* (@ ins color) t0))
    (setf color (vec4 (.xyz (@ ins color)) a))
    #++ (setf color (vec4 a))
    #++(setf color (vec4 (vec3 (length (@ ins bary))) 1))))
