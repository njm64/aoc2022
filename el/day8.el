(require 'aoc)

(setq aoc-day 8)

(defun parse-line (line)
  (vconcat (mapcar #'digit-char-p line)))

(defun aoc-parse (lines)
  (vconcat (mapcar #'parse-line lines)))

(defun move (pos dir)
  (seq-let (x y) pos
    (pcase dir
      (:left (list (1- x) y))
      (:right (list (1+ x) y))
      (:up (list x (1- y)))
      (:down (list x (1+ y))))))

(defun tree-height (forest pos)
  (apply #'matrix-ref forest pos))

(defun in-range (forest pos)
  (seq-let (w h) (matrix-dimensions forest)
    (seq-let (x y) pos
      (and (>= x 0) (>= y 0) (< x w) (< y h)))))

(defun scan (forest x y dir)
  (let (positions (p (list x y)) (max-height -1))
    (while (in-range forest p)
      (let ((h (tree-height forest p)))
        (when (> h max-height)
          (push p positions)
          (setf max-height h)))
      (setf p (move p dir)))
    positions))

(defun map-n (f n)
  (mapcar f (number-sequence 0 (1- n))))

(defun edges (f)
  (seq-let (w h) (matrix-dimensions f)
    (nconc (map-n (lambda (x) (list x 0 :down)) w)
           (map-n (lambda (x) (list x (1- h) :up)) w)
           (map-n (lambda (y) (list 0 y :right)) h)
           (map-n (lambda (y) (list (1- w) y :left)) h))))

(defun count-trees (f p dir)
  (let ((h (tree-height f p)) (count 0))
    (setq p (move p dir))
    (while (and p (in-range f p))
      (setq count (1+ count))
      (setq p (and (< (tree-height f p) h) (move p dir))))
    count))

(defun visibility-score (f p)
  (* (count-trees f p :up)
     (count-trees f p :down)
     (count-trees f p :left)
     (count-trees f p :right)))

(defun aoc-part1 (f)
  (thread-last (edges f)
               (seq-mapcat (lambda (args) (apply #'scan f args)))
               (seq-uniq)
               (length)))

(defun aoc-part2 (f)
  (thread-last (matrix-coords f)
               (mapcar (lambda (pos) (visibility-score f pos)))
               (seq-max)))

