(defpackage :day14
     (:use :cl)
     (:shadowing-import-from :arrow-macros :->>))

(in-package :day14)

(defun parse-segment (s) (->> s (str:split ",") (mapcar #'parse-integer)))
(defun parse-line (s) (->> s (str:split " -> ") (mapcar #'parse-segment)))
(defun parse-input (lines) (mapcar #'parse-line lines))

(defun plot-segment (cave a b)
  (destructuring-bind (xa ya) a
    (destructuring-bind (xb yb) b
      (cond
        ((= xa xb)
         (loop for y from (min ya yb) to (max ya yb) do
           (setf (aref cave xa y) t)))
        ((= ya yb)
         (loop for x from (min xa xb) to (max xa xb) do
           (setf (aref cave x ya) t)))
        (t (error "Invalid line"))))))

(defun max-y (lines)
  (loop for line in lines
        maximize (loop for (x y) in line maximize y)))

(defun make-cave (lines)
  (let* ((height (+ 3 (max-y lines)))
         (cave (make-array (list 1000 height) :initial-element nil)))
    (loop for line in lines do
      (loop for a in line and b = a when b do
        (plot-segment cave a b)))
    cave))

(defun add-floor (cave)
  (destructuring-bind (w h) (array-dimensions cave)
    (plot-segment cave (list 0 (1- h)) (list (1- w) (1- h)))))

(defun clearp (cave x y)
  (not (aref cave x y)))

(defun drop-sand (cave)
  (let ((x 500) (y 0) (height (array-dimension cave 1)))
    (loop while (and (< y (1- height)) (null (aref cave x y))) do
      (cond
        ((clearp cave x (1+ y))       (incf y))
        ((clearp cave (1- x) (1+ y))  (decf x) (incf y))
        ((clearp cave (1+ x) (1+ y))  (incf x) (incf y))
        (t
         (setf (aref cave x y) t)
         (return-from drop-sand t))))))

(defun part1 (rocks)
  (let ((cave (make-cave rocks)))
    (loop while (drop-sand cave) count t)))

(defun part2 (rocks)
  (let ((cave (make-cave rocks)))
    (add-floor cave)
    (loop while (drop-sand cave) count t)))

