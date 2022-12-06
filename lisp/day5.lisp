(defpackage :day5
  (:use :cl :split-sequence :alexandria))

(in-package :day5)

(defun parse-crate (line col)
  (let ((c (char line (1+ (* col 4)))))
    (when (upper-case-p c)
      c)))

(defun parse-stacks (lines)
  (let* ((cols (length (str:words (lastcar lines))))
         (stacks (make-array cols :initial-element nil)))
    (loop for line in (reverse (butlast lines)) do
      (loop for col below cols do
        (when-let (crate (parse-crate line col))
          (push crate (aref stacks col)))))
    stacks))

(defun parse-rule (s)
  (let ((w (str:words s)))
    (mapcar #'parse-integer
            (list (elt w 1) (elt w 3) (elt w 5)))))

(defun parse-input (lines)
  (destructuring-bind (stacks rules) (split-sequence-if #'str:emptyp lines)
    (list (parse-stacks stacks)
          (mapcar #'parse-rule rules))))

(defun pop-crate (stacks i) (pop (aref stacks (1- i))))
(defun push-crate (stacks i c) (push c (aref stacks (1- i))))

(defun part1 (input)
  (destructuring-bind (stacks rules) input
    (setf stacks (copy-array stacks))
    (loop for (n src dst) in rules do
      (loop repeat n do (push-crate stacks dst (pop-crate stacks src))))
    (map 'string #'first stacks)))

(defun part2 (input)
  (destructuring-bind (stacks rules) input
    (setf stacks (copy-array stacks))
    (loop for (n src dst) in rules do
      (let ((tmp))
        (loop repeat n do (push (pop-crate stacks src) tmp))
        (loop for c in tmp do (push-crate stacks dst c))))
    (map 'string #'first stacks)))
