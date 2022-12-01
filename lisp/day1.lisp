(defpackage :day1
  (:use :cl :split-sequence)
  (:shadowing-import-from :arrow-macros :-<>> <>))

(in-package :day1)

(defun parse-group (lines)
  (mapcar #'parse-integer lines))

(defun parse-input (lines)
  (mapcar #'parse-group (split-sequence-if #'str:emptyp lines)))

(defun sum (lst)
  (reduce #'+ lst))

(defun part1 (calories)
  (reduce #'max (mapcar #'sum calories)))

(defun part2 (calories)
  (-<>> calories
    (mapcar #'sum)
    (sort <> #'>)
    (subseq <> 0 3)
    (reduce #'+)))
