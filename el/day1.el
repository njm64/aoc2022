(require 'aoc)

(setq aoc-day 1)

(defun parse-group (lines)
  (mapcar 'string-to-number lines))

(defun aoc-parse (lines)
  (mapcar 'parse-group (split-list 'string-empty-p lines)))

(defun aoc-part1 (calories)
  (seq-reduce 'max (mapcar 'sum calories) 0))

(defun aoc-part2 (calories)
  (sum (seq-take
        (sort (mapcar 'sum calories) '>)
        3)))

