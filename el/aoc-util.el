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
(defun digit-char-p (c) (and (<= ?0 c ?9) (- c ?0)))

(defun words (s)
  (string-split s " "))

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


(defun matrix-width (m) (length (aref m 0)))
(defun matrix-height (m) (length m))
(defun matrix-dimensions (m) (list (matrix-width m) (matrix-height m)))
(defun matrix-ref (m x y) (aref (aref m y) x))

(defun matrix-coords (m)
  (seq-let (w h) (matrix-dimensions f)
    (let (coords)
      (dotimes (x w)
        (dotimes (y h)
          (push (list x y) coords)))
      coords)))

(provide 'aoc-util)
