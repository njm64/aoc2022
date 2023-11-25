(require 'aoc)

(setq aoc-day 3)

(defun aoc-parse (lines)
  (mapcar (lambda (s) (append s nil)) lines))

(defun char-priority (c)
  (cond ((lower-case-p c) (+ (- c ?a) 1))
        ((upper-case-p c) (+ (- c ?A) 27))))

(defun split-in-half (lst)
  (seq-split lst (/ (length lst) 2)))

(defun backpack-score (backpack)
  (thread-last backpack
               (split-in-half)
               (apply 'seq-intersection)
               (delete-dups)
               (mapcar 'char-priority)
               (sum)))

(defun group-score (group)
  (char-priority (car (seq-reduce 'seq-intersection (cdr group) (car group)))))

(defun aoc-part1 (backpacks)
  (sum (mapcar 'backpack-score backpacks)))

(defun aoc-part2 (backpacks)
  (sum (mapcar 'group-score (seq-split backpacks 3))))




