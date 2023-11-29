;; -*- lexical-binding: t; -*-
;; Need lexical binding here for parse-op

(require 'aoc)
(require 'cl-lib)

(setq aoc-day 11)

(cl-defstruct monkey items op div throw-to count)

(defun parse-items (s)
  (let ((items (string-trim-left s ".*: ")))
    (mapcar #'string-to-number (split-string items ", "))))

(defun parse-div (s)
  (string-to-number (nth 3 (words s))))

(defun parse-throw (s)
  (string-to-number (nth 5 (words s))))

(defun parse-op (s)
  (pcase (seq-drop (words s) 4)
    (`("*" "old") (lambda (i) (* i i)))
    (`("*" ,n) (lambda (i) (* i (string-to-number n))))
    (`("+" ,n) (lambda (i) (+ i (string-to-number n))))))

(defun parse-monkey (lines)
  (make-monkey :items (nreverse (parse-items (nth 1 lines)))
               :op (parse-op (nth 2 lines))
               :div (parse-div (nth 3 lines))
               :throw-to (list (parse-throw (nth 4 lines))
                               (parse-throw (nth 5 lines)))
               :count 0))

(defun aoc-parse (lines)
  (mapcar #'parse-monkey (split-list #'string-empty-p lines)))

(defun get-dst (m item)
  (let ((div (monkey-div m))
        (throw-to (monkey-throw-to m)))
    (if (zerop (mod item div))
        (car throw-to)
      (cadr throw-to))))

(defun run-round (monkeys reduce-worry)
  (seq-doseq (m monkeys)
    (dolist (item (reverse (monkey-items m)))
      (let* ((w (funcall (monkey-op m) item))
             (new-item (funcall reduce-worry w))
             (dst-monkey (aref monkeys (get-dst m new-item))))
        (setf (monkey-count m) (1+ (monkey-count m)))
        (push new-item (monkey-items dst-monkey))))
    (setf (monkey-items m) nil)))

(defun run (input rounds reduce-worry)
  (let ((monkeys (vconcat (mapcar #'copy-monkey input))))
    (dotimes (i rounds)
      (run-round monkeys reduce-worry))
    (let ((counts (sort (seq-map #'monkey-count monkeys) #'>)))
      (* (elt counts 0) (elt counts 1)))))

(defun aoc-part1 (input)
  (run input 20 (lambda (w) (floor w 3))))

(defun aoc-part2 (input)
  (let ((p (seq-reduce #'* (mapcar #'monkey-div input) 1)))
    (run input 10000 (lambda (w) (mod w p)))))

