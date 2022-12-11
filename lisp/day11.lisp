(defpackage :day11
  (:use :cl :split-sequence :alexandria :trivia)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day11)

(defclass monkey ()
  ((items :initarg :items :accessor monkey-items)
   (op :initarg :op)
   (div :initarg :div :accessor monkey-div)
   (throw-to :initarg :throw-to)
   (count :initform 0 :accessor monkey-count)))

(defun parse-items (s)
  (->> (second (str:split ": " s))
    (str:split ", ")
    (mapcar #'parse-integer)))

(defun parse-div (s) (parse-integer (nth 3 (str:words s))))
(defun parse-throw (s) (parse-integer (nth 5 (str:words s))))

(defun parse-op (s)
  (match (subseq (str:words s) 4)
    ((list "*" "old") (lambda (i) (* i i)))
    ((list "*" n) (lambda (i) (* i (parse-integer n))))
    ((list "+" n) (lambda (i) (+ i (parse-integer n))))))

(defun parse-monkey (lines)
  (make-instance 'monkey
                 :items (reverse (parse-items (nth 1 lines)))
                 :op (parse-op (nth 2 lines))
                 :div (parse-div (nth 3 lines))
                 :throw-to (list (parse-throw (nth 4 lines))
                                 (parse-throw (nth 5 lines)))))

(defun clone-monkey (m)
  (with-slots (items op div throw-to) m
    (make-instance 'monkey
                   :items items
                   :op op
                   :div div
                   :throw-to throw-to)))

(defun parse-input (lines)
  (mapcar #'parse-monkey (split-sequence-if #'str:emptyp lines)))

(defun get-dst (m item)
  (with-slots (div throw-to) m
    (if (zerop (mod item div))
        (first throw-to)
        (second throw-to))))

(defun run-round (monkeys reduce-worry)
  (loop for m across monkeys do
    (with-slots (items op count) m
      (loop for item in (reverse items) do
        (let* ((new-item (funcall reduce-worry (funcall op item)))
               (dst-monkey (aref monkeys (get-dst m new-item))))
          (incf count)
          (push new-item (monkey-items dst-monkey))))
      (setf items nil))))

(defun run (input rounds reduce-worry)
  (let ((monkeys (map 'vector #'clone-monkey input)))
    (loop repeat rounds do (run-round monkeys reduce-worry))
    (let ((counts (sort (map 'list #'monkey-count monkeys) #'>)))
      (* (first counts) (second counts)))))

(defun part1 (input)
  (run input 20 (lambda (w) (floor w 3))))

(defun part2 (input)
  (let ((p (reduce #'* (mapcar #'monkey-div input))))
    (run input 10000 (lambda (w) (mod w p)))))
