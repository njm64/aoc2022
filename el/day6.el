(require 'aoc)

(setq aoc-day 6)

(defun aoc-parse (lines)
  (append (car lines)))

(defun find-marker (buf len)
  (let ((s buf) (n 0))
    (while (/= len (length (seq-uniq (seq-take s len))))
      (setq s (cdr s) n (1+ n)))
    (+ n len)))

(defun aoc-part1 (s) (find-marker s 4))
(defun aoc-part2 (s) (find-marker s 14))

