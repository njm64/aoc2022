(require 'subr-x)

(defun parent-dir (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties
     (point-min)
     (1- (point-max)))))

(defun lower-case-p (c) (<= ?a c ?z))
(defun upper-case-p (c) (<= ?A c ?Z))

(defun sum (lst)
  (seq-reduce #'+ lst 0))

(defun split-list (f list)
  (let (groups group)
    (dolist (e list)
      (if (funcall f e)
          (progn
            (push (nreverse group) groups)
            (setq group nil))
        (push e group)))
    (when list
      (push (nreverse group) groups))
    (nreverse groups)))

(provide 'aoc-util)
