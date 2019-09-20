(in-package 3b-glim/common)

(deftype octet () '(unsigned-byte 8))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype octet-vector () '(simple-array octet (*)))
(deftype u16-vector () '(simple-array u16 (*)))
(deftype u32-vector () '(simple-array u32 (*)))
(deftype mat4x4 () '(simple-array single-float (16)))


(declaim (inline f c4 v2 v3 v4))
(defun f (x) (coerce x 'single-float))
(deftype v2 () '(simple-array single-float (2)))
(deftype v3 () '(simple-array single-float (3)))
(deftype v4 () '(simple-array single-float (4)))
(defun c4 (r &optional (g 0.0) (b 0.0) (a 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f r) (f g) (f b) (f a))))
(defun v2 (x &optional (y 0.0))
  (make-array 2 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z))))
(defun v3 (x &optional (y 0.0) (z 0.0))
  (make-array 3 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z))))
(defun v4 (x &optional (y 0.0) (z 0.0) (w 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z) (f w))))
