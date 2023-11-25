(require 'aoc)

(setq aoc-day 5)

(defun parse-crate (line col)
  (let ((c (aref line (1+ (* col 4)))))
    (when (upper-case-p c)
      c)))

(defun parse-stacks (lines)
  (let* ((cols (length (string-split (car (last lines)))))
         (stacks (make-vector cols nil)))
    (dolist (line (reverse (butlast lines)))
      (dotimes (col cols)
        (when-let (crate (parse-crate line col))
          (push crate (aref stacks col)))))
    stacks))

(defun parse-rule (s)
  (let ((w (string-split s " ")))
    (mapcar 'string-to-number
            (list (elt w 1) (elt w 3) (elt w 5)))))

(defun aoc-parse (lines)
  (seq-let (stacks rules) (split-list 'string-empty-p lines)
    (list (parse-stacks stacks)
          (mapcar 'parse-rule rules))))

(defun pop-crate (stacks i)
  (pop (aref stacks (1- i))))

(defun push-crate (stacks i c)
  (push c (aref stacks (1- i))))

(defun aoc-part1 (input)
  (seq-let (stacks rules) input
    (setq stacks (copy-sequence stacks))
    (dolist (rule rules)
      (seq-let (n src dst) rule
        (dotimes (i n)
          (push-crate stacks dst (pop-crate stacks src)))))
    (concat (seq-map 'seq-first stacks))))

(defun aoc-part2 (input)
  (seq-let (stacks rules) input
    (setq stacks (copy-sequence stacks))
    (dolist (rule rules)
      (seq-let (n src dst) rule
        (let (tmp)
          (dotimes (i n)
            (push (pop-crate stacks src) tmp))
          (dolist (c tmp)
            (push-crate stacks dst c)))))
    (concat (seq-map 'seq-first stacks))))


