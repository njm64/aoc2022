(defpackage :day12
  (:use :cl))

(in-package :day12)

(defconstant +cost-closed+ -1)

(defun parse-input (lines)
  (let ((h (length (first lines)))
        (w (length lines))
        (data (mapcar (lambda (s) (coerce s 'list)) lines)))
    (make-array (list w h) :initial-contents data)))

(defun mapref (m pos) (apply #'aref m pos))
(defun (setf mapref) (val m pos) (setf (apply #'aref m pos) val))

(defun char-height (c) (- (char-code c) (char-code #\a)))

(defun get-height (m pos)
  (let ((c (mapref m pos)))
    (cond
      ((eql c #\S) (char-height #\a))
      ((eql c #\E) (char-height #\z))
      ((lower-case-p c) (char-height c)))))

(defun can-travel (m src dst &key reverse)
  (if reverse
      (<= (get-height m src) (1+ (get-height m dst)))
      (<= (get-height m dst) (1+ (get-height m src)))))

(defun in-range (m pos)
  (destructuring-bind (width height) (array-dimensions m)
    (destructuring-bind (x y) pos
      (and (>= x 0) (< x width)
           (>= y 0) (< y height)))))

(defun possible-neighbours (pos)
  (loop for d in '((0 1) (0 -1) (1 0) (-1 0))
        collect (mapcar #'+ pos d)))

(defun neighbours (m src &key reverse)
  (loop for dst in (possible-neighbours src)
        when (and (in-range m dst) (can-travel m src dst :reverse reverse))
          collect dst))

(defun find-path (m src goal-chr &key reverse)
  (let ((cost-map (make-array (array-dimensions m) :initial-element nil))
        (q (make-instance 'cl-heap:priority-queue)))
    (cl-heap:enqueue q src 0)
    (setf (mapref cost-map src) 0)
    (loop
      (let* ((pos (cl-heap:dequeue q))
             (cost (mapref cost-map pos)))
        (when (eql (mapref m pos) goal-chr)
          (return cost))
        (unless (eql cost +cost-closed+)
          (loop for neighbour-pos in (neighbours m pos :reverse reverse) do
            (let ((prev-cost (mapref cost-map neighbour-pos))
                  (new-cost (1+ cost)))
              (when (or (null prev-cost) (< new-cost prev-cost))
                (setf (mapref cost-map neighbour-pos) new-cost)
                (cl-heap:enqueue q neighbour-pos new-cost))))
          (setf (mapref cost-map pos) +cost-closed+))))))

(defun find-chr (m c)
  (destructuring-bind (width height) (array-dimensions m)
    (loop for x below width do
      (loop for y below height do
        (if (eql (aref m x y) c)
            (return-from find-chr (list x y)))))))

(defun part1 (m)
  (find-path m (find-chr m #\S) #\E))

(defun part2 (m)
  ;; Just search backwards from end, until we find an 'a'
  (find-path m (find-chr m #\E) #\a :reverse t))


