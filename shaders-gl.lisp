(defpackage #:3b-glim/gl-shaders
  (:use #:3bgl-glsl/cl)
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
  (light-direction :vec3))



(defun vertex ()
  (let* ((mv-pos (* mv position))
         (eye-dir (- (vec3 mv-pos)))
         (mode (.x flags))
         (dxy (vec4 0 0 0 0))
         (pp (* proj mv-pos)))
    (case mode
      (0 ;; tri/quad. do nothing, but test first in hopes of being faster
       ;; workaround for type inference bug
       0)
      (1
       ;; points
       (let ((dist (length (vec3 mv-pos)))
             (corner (.z flags)))
         (case corner
           (0
            (setf dxy (vec4 -1 -1 0 0)))
           (1
            (setf dxy (vec4 -1 1 0 0)))
           (2
            (setf dxy (vec4 1 1 0 0)))
           (3
            (setf dxy (vec4 1 -1 0 0)))
           (t
            (setf dxy (vec4 33 33 0 0))))
         (setf (.xy dxy) (* (.zw dims) point-size (.xy dxy)))
         (setf dxy (* dxy (.w pp))))
       0)
      (2 ;; line
       (let* ((dist (length (vec3 mv-pos)))
              (corner (.z flags))
              (tn (normalize (* (mat3 mv) (.xyz tangent))))
              (tx (cross tn
                         (vec3 0 0 1))))
         (case corner
           (0
            (setf dxy (vec4 -1 -1 0 0)))
           (1
            (setf dxy (vec4 -1 1 0 0)))
           (2
            (setf dxy (vec4 1 1 0 0)))
           (3
            (setf dxy (vec4 1 -1 0 0)))
           (t
            (setf dxy (vec4 33 33 0 0))))
         (setf dxy (vec4 (+ #++(* (.x dxy) (.yx tx))
                            0
                            (* (.y dxy) (.xy tx)))
                         0 0))
         (setf (.xy dxy) (* 1  (.xy dxy)))
         (when (< (abs (length dxy)) 2)
           (setf dxy (* 2 (normalize dxy))))
         (setf (.xy dxy)
               (* (.zw dims) (.xy dxy)))

         (setf dxy (* dxy (.w pp))))
       0)
      (t 0))
    (setf gl-position (+ pp dxy))
    (setf (@ outs normal) (* (mat3 normal-matrix) normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (.xyz texture0)
          (@ outs uv2) (.xyz texture1)
          (@ outs color) color
          (@ outs color2) secondary-color
          ;; interpolated lighting parameters
          (@ outs light-direction) (- light-position (vec3 mv-pos))
          (@ outs eye-direction) eye-dir)))

(output color :vec4 :stage :fragment)

(defun fragment ()
  ;; normalize the interpolated normal, since interpolation doesn't
  ;; preserve length
  #++(let* ((normal (normalize (@ ins normal)))
            ;; same for eye direction and light direction
            (eye-direction (normalize (@ ins eye-direction)))
            (light-direction (normalize (@ ins light-direction)))
            ;; calculate some intermediate values
            (l-dot-n (clamp (dot light-direction normal) 0 1))
            (r (reflect (- light-direction) normal))
            (r-dot-v (clamp (dot r eye-direction) 0 1))
            (distance (length (@ ins eye-direction)))
            (uv (@ ins uv))
            #++    (t0 (case tex-mode0
                         (1
                          (texture tex0-1 (.x uv)))
                         (2
                          (texture tex0-2 (.xy uv)))
                         (3
                          (texture tex0-3 (.xyz uv)))
                         (t
                          (vec4 1 1 1 1)))))
       #++(setf color  (* (@ ins color) t0)))
  (setf color (vec4 (.xyz (@ ins color)) 1)))
