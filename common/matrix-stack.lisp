(in-package #:3b-glim/common)

(defparameter *matrix-stacks* (make-hash-table))
(defparameter *matrices* (make-hash-table))
(defparameter *current-matrix* nil)

(declaim (inline v))
(defun v (l)
  (etypecase l
    (sb-cga:vec
     l)
    (cons
     (sb-cga:vec (float (pop l) 1f0) (float (pop l) 1f0) (float (pop l) 1f0)))
    (vector
     (sb-cga:vec (float (aref l 0) 1f0) (float (aref l 1) 1f0)
                 (float (aref l 2) 1f0)))))

(defun ensure-matrix (id)
  (or (gethash id *matrices*)
      (setf (gethash id *matrices*)
            (sb-cga:identity-matrix))))

(defun matrix-mode (id)
  (setf *current-matrix* (ensure-matrix id)))

(defun push-matrix (id)
  (let ((old (ensure-matrix id)))
    (push (copy-seq old) (gethash id *matrix-stacks*))))

(defun pop-matrix (id)
  (unless (gethash id *matrix-stacks*)
    (cerror "keep existing" "Matrix stack underflow in stack ~s?" id))
  (let ((old (gethash id *matrices*)))
    (setf (gethash id *matrices*)
          (or (pop (gethash id *matrix-stacks*))
              old
              (sb-cga:identity-matrix)))
    (when (eql *current-matrix* old)
      (setf *current-matrix* (gethash id *matrices*))
      #++(replace *current-matrix* (gethash id *matrices*)))))

(defun load-identity ()
  (replace *current-matrix* (sb-cga:identity-matrix)))

(defun load-matrix (m)
  (replace *current-matrix* m))

(defun copy-matrix (&optional m)
  (if m
      (copy-seq (ensure-matrix m))
      (copy-seq *current-matrix*)))

(declaim (inline deg-to-rad))
(defun deg-to-rad (a)
  (* a #.(coerce (/ pi 180) 'single-float)))

(defun rotate (theta x y z)
  (replace
   *current-matrix*
   (sb-cga:matrix*
    *current-matrix*
    (sb-cga:rotate-around (sb-cga:normalize (sb-cga:vec (f x) (f y) (f z)))
                          (deg-to-rad (f theta))))))

(defun translate (x y z)
  (replace *current-matrix*
           (sb-cga:matrix*
            *current-matrix*
            (sb-cga:translate* (f x) (f y) (f z)))))

(defun scale (x y z)
  (replace *current-matrix*
           (sb-cga:matrix*
            *current-matrix*
            (sb-cga:scale* (f x) (f y) (f z)))))

(defun frustum (left right bottom top near far)
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near))
        (2near (* 2 near)))
    (replace
     *current-matrix*
     (sb-cga:matrix (/ 2near r-l) 0.0 (/ (+ right left) r-l) 0.0
                    0.0 (/ 2near t-b) (/ (+ top bottom) t-b) 0.0
                    0.0 0.0 (- (/ (+ far near) f-n)) (/ (* -2 far near) f-n)
                    0.0 0.0 -1.0 0.0))))

(defun perspective (fovy-degrees aspect z-near z-far)
  (let ((f (float (/ (tan (/ (deg-to-rad fovy-degrees) 2))) 1.0))
        (dz (float (- z-near z-far) 1.0)))
    (replace
     *current-matrix*
     (sb-cga:matrix (/ f aspect) 0.0 0.0 0.0
                    0.0 f 0.0 0.0
                    0.0 0.0 (/ (+ z-near z-far) dz) (/ (* 2 z-near z-far) dz)
                    0.0 0.0 -1.0 0.0))))

(defun ortho (left right bottom top near far)
  (let ((r-l (float (- right left)))
        (t-b (float (- top bottom)))
        (f-n (float (- far near))))
    (replace
     *current-matrix*
     (sb-cga:matrix*
      *current-matrix*
      (sb-cga:matrix (/ 2 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
                     0.0 (/ 2 t-b) 0.0 (- (/ (+ top bottom) t-b))
                     0.0 0.0 (/ -2 f-n) (- (/ (+ far near) f-n))
                     0.0 0.0 0.0 1.0)))))

(defun look-at (eye target up)
  (let* ((eye (v eye))
         (target (v target))
         (up (v up))
         (z (sb-cga:normalize (sb-cga:vec- target eye)))
         (x (sb-cga:normalize (sb-cga:cross-product z up)))
         (y (sb-cga:cross-product x z)))
    (replace
     *current-matrix*
     (sb-cga:matrix*
      *current-matrix*
      (sb-cga:transpose-matrix
       (sb-cga:matrix (aref x 0) (aref y 0) (- (aref z 0)) 0.0
                      (aref x 1) (aref y 1) (- (aref z 1)) 0.0
                      (aref x 2) (aref y 2) (- (aref z 2)) 0.0
                      0.0 0.0 0.0 1.0))
      (sb-cga:translate (sb-cga:vec* eye -1.0))))))

(defmacro with-pushed-matrix ((id) &body body)
  (alexandria:once-only (id)
    `(progn
       (push-matrix ,id)
       (unwind-protect (progn ,@body)
         (pop-matrix ,id)))))

(defmacro with-balanced-matrix-stacks (() &body body)
  `(unwind-protect
        (progn ,@body)
     (maphash (lambda (k v) (declare (ignore k)) (assert (<= (length v) 1)))
              *matrix-stacks*)))

(defmacro with-matrix-stacks (() &body body)
  `(let ((*matrix-stacks* (make-hash-table))
        (*matrices* (make-hash-table)))
    ,@body))
