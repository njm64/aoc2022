(require 'aoc-util)

(defvar aoc-day 0)
(defvar aoc-test nil)

(defun aoc-input-dir ()
  (if aoc-test
      "../test"
    "../input"))

(defun aoc-filename ()
  (concat (aoc-input-dir) "/day" (number-to-string aoc-day) ".txt"))

(defun aoc-read ()
  (let ((filename (aoc-filename)))
    (split-string (read-file filename) "\n")))

(defun aoc-time ()
  (float-time (current-time)))

(defun aoc-run-part (n f input)
  (let ((start-time (aoc-time))
        (result (funcall f input)))
    (princ (format "Day %2d Part %d: %-20s %0.6fs\n"
                   aoc-day n
                   result (- (aoc-time) start-time)))))
  
(defun aoc-run ()
  (interactive)
  (let ((input (aoc-parse (aoc-read))))
    (aoc-run-part 1 #'aoc-part1 input)
    (aoc-run-part 2 #'aoc-part2 input)
    nil))

(keymap-local-set "C-c C-k" 'eval-buffer)
(keymap-local-set "M-a" 'aoc-run)

(provide 'aoc)


