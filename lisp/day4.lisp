(defpackage :day4
  (:use :cl))

(in-package :day4)

(defun parse-range (r) (mapcar #'parse-integer (str:split "-" r)))
(defun parse-line (line) (mapcar #'parse-range (str:split "," line)))
(defun parse-input (lines) (mapcar #'parse-line lines))

(defun contains (pair)
  (destructuring-bind ((min-a max-a) (min-b max-b)) pair
    (or (<= min-a min-b max-b max-a)
        (<= min-b min-a max-a max-b))))

(defun overlaps (pair)
  (destructuring-bind ((min-a max-a) (min-b max-b)) pair
    (or (<= min-b min-a max-b)
        (<= min-b max-a max-b)
        (<= min-b min-a max-a max-b)
        (<= min-a min-b max-b max-a))))

(defun part1 (pairs) (count-if #'contains pairs))
(defun part2 (pairs) (count-if #'overlaps pairs))
