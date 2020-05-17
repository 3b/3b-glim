(in-package 3b-glim/common)

;; types
(deftype octet () '(unsigned-byte 8))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype s8 () '(signed-byte 8))
(deftype s16 () '(signed-byte 16))
(deftype s32 () '(signed-byte 32))
(deftype octet-vector () '(simple-array octet (*)))
(deftype u16-vector () '(simple-array u16 (*)))
(deftype u32-vector () '(simple-array u32 (*)))
(deftype float-vector (&optional (n '*)) `(simple-array single-float (,n)))
(deftype mat4x4 () '(simple-array single-float (16)))

(deftype v2 () '(simple-array single-float (2)))
(deftype v3 () '(simple-array single-float (3)))
(deftype v4 () '(simple-array single-float (4)))


;; constructors/casts

(declaim (inline f u8 u16 u32 s8 s16 s32
                 c4 v2 v3 v4))

(defun f (x) (coerce x 'single-float))
(defun u8 (x) (coerce x 'u8))
(defun u16 (x) (coerce x 'u16))
(defun u32 (x) (coerce x 'u32))
(defun s8 (x) (coerce x 's8))
(defun s16 (x) (coerce x 's16))
(defun s32 (x) (coerce x 's32))

(defun c4 (r &optional (g 0.0) (b 0.0) (a 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f r) (f g) (f b) (f a))))
(defun v2 (x &optional (y 0.0))
  (make-array 2 :element-type 'single-float
                :initial-contents (list (f x) (f y))))
(defun v3 (x &optional (y 0.0) (z 0.0))
  (make-array 3 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z))))
(defun v4 (x &optional (y 0.0) (z 0.0) (w 1.0))
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f x) (f y) (f z) (f w))))
