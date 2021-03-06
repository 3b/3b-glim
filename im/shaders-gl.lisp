(defpackage #:3b-glim/gl-shaders
  (:use #:3bgl-glsl/cl)
  (:import-from #:3b-glim
                #:mv #:proj #:mvp #:normal-matrix
                #:tex-mode0
                #:tex0-1 #:tex0-2 #:tex0-3
                #:line-width #:point-size #:lights-enabled #:lights
                #:draw-flags)
  (:export #:vertex
           #:fragment))
(in-package #:3b-glim/gl-shaders)

(input position :vec4 :location 0)
(input texture0 :vec4 :location 1)
(input tangent :vec4 :location 2)
(input normal :vec3 :location 3)
(input color :vec4 :location 4)
(input flags :vec4 :location 5)
(input secondary-color :vec3 :location 6)

(uniform mv :mat4) ;; model-view matrix
;;(uniform mvp :mat4) ;; model-view-projection matrix
(uniform proj :mat4) ;; projection matrix
(uniform normal-matrix :mat4)
(uniform tex-mode0 :int)
(uniform tex0-1 :sampler-1d)
(uniform tex0-2 :sampler-2d)
(uniform tex0-3 :sampler-3d)

(uniform line-width :float)
(uniform point-size :float)
(uniform draw-flags :vec4)
(uniform lights-enabled :vec4)

(defstruct -lights
  (position :vec4)
  (ambient :vec4)
  (diffuse :vec4)
  (specular :vec4)
  (spot-dir :vec3)
  (spot-params :vec3)
  (attenuation :vec3))

(uniform lights (-lights 4))

(uniform dims :vec4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec3)
  (normal :vec3)
  (color :vec4)
  (color2 :vec3)
  (uv :vec3)
  (bary :vec4)
  (flags :vec4 :flat))


(defun smooth-tri-bary (corner w b)
  (declare (inout b))
  (setf (aref b (int corner)) 1)
  (setf (.w b) w))

(defun smooth-quad-bary (corner w b)
  (declare (out b))
  (case corner
    (0 (setf (.xyw b) (vec3 -1 1 w)))
    (1 (setf (.xyw b) (vec3 -1 -1 w)))
    (2 (setf (.xyw b) (vec3 1 -1 w)))
    (t (setf (.xyw b) (vec3 1 1 w)))))

(defun points-bary (corner smoothp point-size bary dxy)
  (declare (inout bary)
           (out dxy))
  (let* ((s2 (+ point-size
                (if smoothp 2 0)))
         (s1 (- s2)))
    (case corner
      (0
       (setf (.xyz bary) (vec3 s2 s1 point-size))
       (setf dxy (vec2 1 -1)))
      (1
       (setf (.xyz bary) (vec3 s2 s2 point-size))
       (setf dxy (vec2 1 1)))
      (2
       (setf (.xyz bary) (vec3 s1 s2 point-size))
       (setf dxy (vec2 -1 1)))
      (3
       (setf (.xyz bary) (vec3 s1 s1 point-size))
       (setf dxy (vec2 -1 -1)))
      (t
       (setf dxy (vec2 33 33))))
    (setf dxy (* (.zw dims) s2 dxy))))

(defun lines-bary (corner smoothp tl tx line-width bary dxy)
  (declare (out dxy bary))
  (let* ((s2 (+ line-width (if smoothp 2 0)))
         (s1 (- s2)))
    (if smoothp
        (let ((ty (vec2 (- (.y tx)) (.x tx))))
          (case corner
            (0
             (setf bary (vec4 (- s1 tl) s1 line-width tl)))
            (1
             (setf tx (- tx))
             (setf bary (vec4 (- s1 tl) s2 line-width tl)))
            (2
             (setf bary (vec4 (+ s2 tl) s2 line-width tl)))
            (3
             (setf tx (- tx))
             (setf bary (vec4 (+ s2 tl) s1 line-width tl))))
          (setf dxy (+ tx ty)))
        (progn
          (case corner
            (1
             (setf tx (- tx)))
            (3
             (setf tx (- tx))))
          (setf dxy (normalize (.xy tx)))))
    (setf dxy (* s2 dxy))
    (when (< (abs (length dxy)) 2)
      (setf dxy (* 2 (normalize dxy))))
    (setf dxy (* (.zw dims) dxy))))


(defun vertex ()
  (let* ((mv-pos (* mv position))
         (eye-dir (- (vec3 mv-pos)))
         (mode (.x flags))
         (dxy (vec2 0 0))
         (pp (* proj mv-pos))
         (bary (vec4 0))
;;; fixme: calculate and pass proper normal matrix from host
         (nm (mat3 mv)))
    (case mode
      (0 ;; tri
       ;; wireframe modes
       (when (or (any (equal (vec2 2) (.xy draw-flags)))
                 (any (equal (vec2 3) (.xy draw-flags))))
         (smooth-tri-bary (.z flags) line-width bary))
       ;; workaround for type inference bug
       0)
      (3 ;; quads
       (when (or (any (equal (vec2 2) (.xy draw-flags)))
                 (any (equal (vec2 3) (.xy draw-flags))))
         (smooth-quad-bary (.z flags) line-width bary))
       ;; workaround for type inference bug
       0)
      (1
       ;; points
       (let* ((corner (.z flags)))
         (points-bary corner (= 1 (.x draw-flags))
                      point-size
                      bary dxy)
         (setf dxy (* dxy (.w pp))))
       0)
      (2 ;; line
       (let* ((mv-p2 (* mv (vec4 (.xyz tangent) 1)))
              (pp2 (* proj mv-p2))
              (tn (- (/ (.xyz pp2) (.w pp2))
                     (/ (.xyz pp) (.w pp))))
              (tl (length (* (.xy tn) (/ (.zw dims)) 0.5)))
              (tx (normalize (.xy (cross tn (.xyz mv-pos))))))
         (lines-bary (.z flags)
                     (= 1 (.x draw-flags))
                     tl tx
                     line-width
                     bary dxy
                     )
         (setf dxy (* dxy (.w pp))))
       0)
      (t
       0))
    (setf gl-position (vec4 (+ (.xy pp) dxy) (.zw pp)))
    (setf (@ outs normal) (* nm normal)
          (@ outs position) (vec3 mv-pos)
          (@ outs uv) (.xyz texture0)
          (@ outs color) color
          (@ outs color2) secondary-color
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
    (return (* e (min ps 1)))))

(defun smooth-line (bary)
  (let* ((ps (.z bary))
         (tl (.w bary))
         (x (abs (.x bary)))
         (y (abs (.y bary)))
         (s1 (* #.(sqrt 2.0)
                (length (vec2 (dfdx y) (dfdy y)))))
         (r (length (vec2 (- x tl) y)))
         (s2 (* #.(sqrt 2.0)
                (length (vec2 (dfdx r) (dfdy r)))))
         (mps (max ps 1.5))
         #++(s (fwidth r))
         (e (if (< x tl)
                (smooth-step (+ mps s1) (- mps s1) y)
                (smooth-step (+ mps s2) (- mps s2) r)))
         #++(e (if (< x (* 1 tl))
                   (step y (max 1 ps))
                   (step r (max 1 ps)))))
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
    (return (* m 1 (min w 1)))))

(defun wire-quad (bary)
  (let* ((d  (fwidth (.xy bary)))
         (dx (abs (dfdx (.xy bary))))
         (dy (abs (dfdy (.xy bary))))
         (w (.w bary))
         (w1 (- 1 (* (max 2 (.w bary))
                     (vec2 (max (.x dx) (.x dy))
                           (max (.y dx) (.y dy))))))
         #++(a (step (vec2 (- 1 w))
                     (abs (.xy bary))))
         (a (smooth-step (- w1 d)
                         (+ w1 d)
                         (abs (.xy bary))))
         (m (max (.x a) (.y a))))
    (return (* m (min w 1)))))


(defun smoothing (draw-flag primitive bary)
  (case draw-flag
    ;; normal
    (0 (return 1.0))
    ;; smooth
    (1 (case primitive
         (0 ;; tri
          (return 1.0))
         (3 ;; quad
          (return 1.0))
         (1 ;; points
          (return (smooth-point bary)))
         (2 ;; lines
          (return (smooth-line bary)))
         ;;
         (t (return 1.0))))
    ;; wireframe
    ((2 3)
     (case primitive
       (0                             ;tri
        (return (wire-tri bary)))
       (3                             ;quad
        (return (wire-quad bary)))))
    (t
     (return 1.0)))
  (return 1.0))

(defun ambient-m ()
  ;; todo: check face, material uniforms
  (return (vec4 0.2 0.2 0.2 1)))

(defun diffuse-m (uv)
  ;; todo: check face, material uniforms
  (return
    (* (@ ins color)
       (case tex-mode0
         (2
          (texture tex0-2 (.xy uv)))
         (1
          (texture tex0-1 (.x uv)))
         (3
          (texture tex0-3 (.xyz uv)))
         (t
          (vec4 0.8 0.8 0.8 1))))))

(defun specular-m ()
  ;; todo: check face, material uniforms
  (return (vec4 1 1 1 1)))

(defun emissive-m ()
  ;; todo: check face, material uniforms
  (return (vec4 0 0 0 1)))


(defun ambient-scene ()
  ;; todo: check uniforms
  (return (vec4 0.2 0.2 0.2 1)))

(defun light (l n v eye acm dcm scm)
  (let* ((acli (@ (aref lights l) ambient))
         (dcli (@ (aref lights l) diffuse))
         (scli (@ (aref lights l) specular))
         (pli (@ (aref lights l) position))
         (vpli (if (zerop (.w pli))
                   (normalize (.xyz pli))
                   (normalize (- (.xyz pli) v))))
         (ndotvp (max 0 (dot n vpli)))
         (h (normalize (+ eye vpli)))
         (atti (@ (aref lights l) attenuation))
         (att 1.0)
         (srm 10.0) ;; todo: get from uniform
         (spec (vec4 0)))
    (unless (zerop (.w pli))
      (let ((d (length (- v (.xyz pli)))))
        (setf att (/ (dot (vec3 1 d (* d d)) atti)))))
    ;; todo: spot
    (unless (= ndotvp 0)
      (setf spec
            (* (expt (max 0 (dot n h)) srm) scm scli)))
    (return
      (* att
         (+ (* acm acli)
            (* ndotvp dcm dcli)
            spec)))))

(defun fragment ()
  (let* ((normal (normalize (@ ins normal)))
         (eye-direction (normalize (- (@ ins position))))
         (draw-flag (if gl-front-facing
                        (.x draw-flags)
                        (.y draw-flags)))
         (a (smoothing draw-flag (.x (@ ins flags)) (@ ins bary)))
         (dcm (diffuse-m (@ ins uv)))
         #++(acm (ambient-m))
         (acm dcm)
         (scm (specular-m))
         (ecm (emissive-m))
         (acs (ambient-scene))
         (clight dcm))
    (declare (:float a))
    (case draw-flag
      (3
       (setf color (vec4 (mix (.xyz (@ ins color))
                              (.xyz (@ ins color2))
                              a)
                         1)))
      (t
       (when (<= a 0.0)
         (discard))
       (unless (zerop (.z draw-flags))
         (setf clight (+ ecm (* acm acs)))
         (when gl-front-facing
           (unless (zerop (.x lights-enabled))
             (incf clight (light 0 normal (@ ins position) eye-direction
                                 acm dcm scm)))
           (unless (zerop (.y lights-enabled))
             (incf clight (light 1 normal (@ ins position) eye-direction
                                 acm dcm scm)))
           (unless (zerop (.z lights-enabled))
             (incf clight (light 2 normal (@ ins position) eye-direction
                                 acm dcm scm)))
           (unless (zerop (.w lights-enabled))
             (incf clight (light 3 normal (@ ins position) eye-direction
                                 acm dcm scm)))))

       (setf color (vec4 (if gl-front-facing
                             (.xyz clight)
                             (.xyz (@ ins color2)))
                         a))))))
