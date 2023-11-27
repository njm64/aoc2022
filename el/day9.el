(require 'aoc)

(setq aoc-day 9)

(defun parse-motion (line)
  (seq-let (dir count) (string-split line " ")
    (list (aref dir 0) (string-to-number count))))

(defun aoc-parse (lines)
  (mapcar #'parse-motion lines))

(defun move-head (pos dir)
  (seq-let (x y) pos
    (pcase dir
      (?U (list x (1+ y)))
      (?D (list x (1- y)))
      (?L (list (1- x) y))
      (?R (list (1+ x) y)))))

(defun nudge (from to)
  (cond
   ((= from to) to)
   ((< from to) (1+ from))
   ((> from to) (1- from))))

(defun move-tail (rope-pos tail-pos)
  (seq-let (rx ry) rope-pos
    (seq-let (tx ty) tail-pos
      (cond
       ((> ry (1+ ty)) (list (nudge tx rx) (1+ ty)))
       ((< ry (1- ty)) (list (nudge tx rx) (1- ty)))
       ((> rx (1+ tx)) (list (1+ tx) (nudge ty ry)))
       ((< rx (1- tx)) (list (1- tx) (nudge ty ry)))
       (t tail-pos)))))

(defun rope-reducer (rope tail)
  (cons (move-tail (car rope) tail) rope))

(defun apply-dir (state dir)
  (seq-let (rope history) state
    (let* ((new-head (move-head (car rope) dir))
           (new-rope (seq-reduce #'rope-reducer
                                 (cdr rope)
                                 (list new-head)))
           (new-tail (car new-rope)))
      (list (reverse new-rope) (cons new-tail history)))))

(defun apply-motion (state motion)
  (seq-let (dir count) motion
    (dotimes (i count)
      (setf state (apply-dir state dir)))
    state))

(defun run (rope-length motions)
  (thread-last (list (make-list rope-length '(0 0)) nil)
               (seq-reduce #'apply-motion motions)
               (cadr)
               (seq-uniq)
               (length)))

(defun aoc-part1 (motions) (run 2 motions))
(defun aoc-part2 (motions) (run 10 motions))
