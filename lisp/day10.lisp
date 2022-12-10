(defpackage :day10
  (:use :cl :trivia))

(in-package :day10)

(defun parse-input (lines)
  (mapcar #'str:words lines))

(defun run-program (program)
  (let ((xs) (x 1))
    (loop for instruction in program do
      (match instruction
        ((list "addx" n)
         (push x xs)
         (push x xs)
         (incf x (parse-integer n)))
        ((list "noop")
         (push x xs))))
    (push x xs)
    (reverse xs)))

(defun calc-pixel (cycle x)
  (let ((col (mod (1- cycle) 40)))
    (if (<= (abs (- x col)) 1) #\# #\.)))

(defun get-pixels (xs)
  (loop for x in xs 
        for cycle = 1 then (1+ cycle)
        collect (calc-pixel cycle x)))

(defun print-image (pixels)
  (loop while (>= (length pixels) 40) do
    (format t "~a~%" (coerce (subseq pixels 0 40) 'string))
    (setf pixels (subseq pixels 40))))

(defun part1 (instructions)
  (let* ((xs (run-program instructions))
         (signal-strength (lambda (c) (* c (elt xs (1- c))))))
    (reduce #'+ (mapcar signal-strength '(20 60 100 140 180 220)))))

(defun part2 (instructions)
  (let ((xs (run-program instructions)))
    (print-image (get-pixels xs))))
