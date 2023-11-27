(require 'aoc)

(setq aoc-day 10)

(defun aoc-parse (lines)
  (mapcar #'words lines))

(defun run-program (program)
  (let (xs (x 1))
    (dolist (instruction program)
      (pcase instruction
        (`("addx" ,n)
         (push x xs)
         (push x xs)
         (setf x (+ x (string-to-number n))))
        (`("noop")
         (push x xs))))
    (nreverse xs)))

(defun calc-pixel (cycle x)
  (let ((col (mod (1- cycle) 40)))
    (if (<= (abs (- x col)) 1) ?# ?.)))

(defun get-pixels (xs)
  (seq-map-indexed (lambda (x i) (calc-pixel (1+ i) x)) xs))

(defun print-image (pixels)
  (dolist (line (mapcar #'concat (seq-split pixels 40)))
    (princ line)
    (terpri)))

(defun aoc-part1 (instructions)
  (let ((xs (run-program instructions)))
    (sum (mapcar (lambda (c) (* c (elt xs (1- c))))
                 '(20 60 100 140 180 220)))))

(defun aoc-part2 (instructions)
  (thread-last (run-program instructions)
               (get-pixels)
               (print-image)))

