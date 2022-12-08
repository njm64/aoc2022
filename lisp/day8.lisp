(defpackage :day8
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day8)

(defun parse-line (line)
  (map 'list #'digit-char-p line))

(defun transpose (m)
  (apply #'mapcar 'list m))

(defun parse-input (lines)
  (let ((w (length (first lines)))
        (h (length lines))
        (data (mapcar 'parse-line lines)))
    (make-array (list w h) :initial-contents (transpose data))))

(defun move (pos dir)
  (mapcar #'+ pos dir))

(defun tree-height (forest pos)
  (apply #'aref forest pos))

(defun in-range (forest pos)
  (destructuring-bind (w h) (array-dimensions forest)
    (destructuring-bind (x y) pos
      (and (>= x 0) (>= y 0) (< x w) (< y h)))))

(defun scan (forest pos dir)
  (let ((positions)
        (max-height -1))
    (loop for p = pos then (move p dir)
          while (in-range forest p) do
            (let ((h (tree-height forest p)))
              (when (> h max-height)
                (push p positions)
                (setf max-height h))))
    positions))

(defun count-trees (forest pos dir)
  (let ((h (tree-height forest pos)))
    (loop for p = (move pos dir) then (move p dir)
          while (in-range forest p)
          count 1
          until (>= (tree-height forest p) h))))

(defun visibility-score (forest pos)
  (->> '((-1 0) (1 0) (0 1) (0 -1))
    (mapcar (lambda (d) (count-trees forest pos d)))
    (reduce #'*)))

(defun part1 (f)
  (destructuring-bind (w h) (array-dimensions f)
    (let ((vis))
      (loop for y below h do
        (appendf vis (scan f (list 0 y) '(1 0)))
        (appendf vis (scan f (list (1- w) y) '(-1 0))))
      (loop for x below w do
        (appendf vis (scan f (list x 0) '(0 1)))
        (appendf vis (scan f (list x (1- h)) '(0 -1))))
      (length (remove-duplicates vis :test 'equal)))))

(defun part2 (f)
  (destructuring-bind (w h) (array-dimensions f)
    (loop for x below w maximize
      (loop for y below h maximize
        (visibility-score f (list x y))))))
