(defpackage :day3
     (:use :cl)
     (:shadowing-import-from :arrow-macros :->>))

(in-package :day3)

(defun parse-input (lines)
  (mapcar (lambda (s) (coerce s 'list)) lines))

(defun char-priority (c)
  (cond ((char<= #\a c #\z) (- (+ (char-code c) 1)  (char-code #\a)))
        ((char<= #\A c #\Z) (- (+ (char-code c) 27) (char-code #\A)))))

(defun split-n (n lst)
  (loop for b = lst then (subseq b n)
        while b
        collect (subseq b 0 n)))

(defun backpack-score (backpack)
  (->> backpack
    (split-n (/ (length backpack) 2))
    (apply #'intersection)
    (remove-duplicates)
    (mapcar #'char-priority)
    (reduce #'+)))

(defun group-score (group)
  (char-priority (first (reduce #'intersection group))))

(defun part1 (backpacks)
  (reduce #'+ (mapcar #'backpack-score backpacks)))

(defun part2 (backpacks)
  (reduce #'+ (mapcar #'group-score (split-n 3 backpacks))))
