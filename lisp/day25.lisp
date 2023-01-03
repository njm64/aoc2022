(defpackage :day25
  (:use :cl))

(in-package :day25)

(defun parse-input (lines) lines)

(defparameter *smap* '((-1 . #\-) (-2 . #\=) (0 . #\0) (1 . #\1) (2 . #\2)))
(defun snafu-int-chr (i) (cdr (assoc i *smap*)))
(defun snafu-chr-int (c) (car (rassoc c *smap*)))

(defun snafu-int (s)
  (loop for d = 0 then (+ (* d 5) (snafu-chr-int c))
        for c across s
        finally (return d)))

(defun int-snafu (i)
  (loop for n = i then (floor (+ n 2) 5)
        for d = (- (mod (+ n 2) 5) 2)
        while (plusp n)
        collect (snafu-int-chr d) into digits
        finally (return (coerce (reverse digits) 'string))))

(defun part1 (snafus)
  (int-snafu (apply #'+ (mapcar #'snafu-int snafus))))

(defun part2 (snafus)
  (declare (ignore snafus)))
