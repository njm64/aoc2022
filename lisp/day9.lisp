(defpackage :day9
  (:use :cl))

(in-package :day9)

(defun parse-motion (line)
  (destructuring-bind (dir count) (str:words line) 
    (list (elt dir 0) (parse-integer count))))

(defun parse-input (lines)
  (mapcar #'parse-motion lines))

(defun move-head (pos dir)
  (destructuring-bind (x y) pos
    (ecase dir
      (#\U (list x (1+ y)))
      (#\D (list x (1- y)))
      (#\L (list (1- x) y))
      (#\R (list (1+ x) y)))))

(defun nudge (from to)
  (cond
    ((= from to) to)
    ((< from to) (1+ from))
    ((> from to) (1- from))))

(defun move-tail (rope-pos tail-pos)
  (destructuring-bind (rx ry) rope-pos
    (destructuring-bind (tx ty) tail-pos
      (cond
        ((> ry (1+ ty)) (list (nudge tx rx) (1+ ty)))
        ((< ry (1- ty)) (list (nudge tx rx) (1- ty)))
        ((> rx (1+ tx)) (list (1+ tx) (nudge ty ry)))
        ((< rx (1- tx)) (list (1- tx) (nudge ty ry)))
        (t tail-pos)))))

(defun rope-reducer (rope tail)
  (cons (move-tail (first rope) tail) rope))

(defun apply-dir (state dir)
  (destructuring-bind (rope history) state
    (let* ((new-head (move-head (first rope) dir))
           (new-rope (reduce #'rope-reducer (rest rope) :initial-value (list new-head)))
           (new-tail (first new-rope)))
      (list (reverse new-rope) (cons new-tail history)))))

(defun apply-motion (state motion)
  (destructuring-bind (dir count) motion
    (loop repeat count finally (return state) do
      (setf state (apply-dir state dir)))))

(defun make-rope (length)
  (loop repeat length collect '(0 0)))

(defun run (rope motions)
  (let ((state (list rope nil)))
    (let ((new-state (reduce #'apply-motion motions :initial-value state)))
      (length (remove-duplicates (second new-state) :test #'equal)))))

(defun part1 (motions) (run (make-rope 2) motions))
(defun part2 (motions) (run (make-rope 10) motions))





