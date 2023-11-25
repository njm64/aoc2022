(require 'aoc)

(setq aoc-day 4)

(defun parse-range (r)
  (mapcar 'string-to-number (string-split r "-")))

(defun parse-line (line)
  (mapcar 'parse-range (string-split line ",")))

(defun aoc-parse (lines)
  (mapcar 'parse-line lines))

(defun contains (pair)
  (seq-let ((min-a max-a) (min-b max-b)) pair
    (or (<= min-a min-b max-b max-a)
        (<= min-b min-a max-a max-b))))

(defun overlaps (pair)
  (seq-let ((min-a max-a) (min-b max-b)) pair
    (or (<= min-b min-a max-b)
        (<= min-b max-a max-b)
        (<= min-b min-a max-a max-b)
        (<= min-a min-b max-b max-a))))

(defun aoc-part1 (pairs) (seq-count 'contains pairs))
(defun aoc-part2 (pairs) (seq-count 'overlaps pairs))

