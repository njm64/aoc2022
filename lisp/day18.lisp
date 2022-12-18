(defpackage :day18
  (:use :cl))

(in-package :day18)

(defun parse-cube (line)
  (mapcar #'parse-integer (str:split "," line)))

(defun parse-input (lines)
  (mapcar #'parse-cube lines))

(defun transpose (lst)
  (apply #'mapcar 'list lst))

(defun get-range (xs)
  (loop for x in xs
        minimizing x into min
        maximizing x into max
        finally (return (cons (1- min) (1+ max)))))

(defun in-range (r x)
  (destructuring-bind (min . max) r
    (<= min x max)))

(defun neighbours (p)
  (destructuring-bind (x y z) p
    (list (list x y (1- z))
          (list x y (1+ z))
          (list x (1- y) z)
          (list x (1+ y) z)
          (list (1- x) y z)
          (list (1+ x) y z))))

(defun make-cube-table (cubes)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for cube in cubes do
      (setf (gethash cube ht) :cube))
    ht))

(defun part1 (cubes)
  (let ((ht (make-cube-table cubes)))
    (loop for cube in cubes
          sum (loop for n in (neighbours cube)
                    count (not (gethash n ht))))))

(defun part2 (cubes)
  (let* ((rs (mapcar #'get-range (transpose cubes)))
         (ht (make-cube-table cubes))
         (start (mapcar #'first rs))
         (stack nil)
         (count 0))
    (push start stack)
    (setf (gethash start ht) :visited)
    (loop for node = (pop stack) while node do
      (loop for n in (neighbours node)
            when (every #'in-range rs n) do
        (case (gethash n ht :unvisited)
          (:cube (incf count))
          (:unvisited
           (setf (gethash n ht) :visited)
           (push n stack)))))
    count))
