(defpackage :day22
  (:use :cl :split-sequence :alexandria))

(in-package :day22)

;; Top-left coordinates of the squares. Layout is different in the test data.
(defparameter *test-squares* '((8 0) (0 4) (4 4) (8 4) (8 8) (12 8)))
(defparameter *squares* '((50 0) (100 0) (50 50) (0 100) (50 100) (0 150)))

(defparameter *test-connections*
  `((1 :left   3 :up    ,#'identity)
    (1 :up     2 :up    ,#'reverse)
    (1 :right  6 :right ,#'reverse)
    (2 :left   6 :down  ,#'reverse)
    (2 :down   5 :down  ,#'reverse)
    (3 :down   5 :left  ,#'reverse)
    (4 :right  6 :up    ,#'reverse)))

(defparameter *connections*
  `((1 :left   4 :left  ,#'reverse)
    (1 :up     6 :left  ,#'identity)
    (2 :up     6 :down  ,#'identity)
    (2 :right  5 :right ,#'reverse)
    (2 :down   3 :right ,#'identity)
    (5 :down   6 :right ,#'identity)
    (4 :up     3 :left  ,#'identity)))

(defparameter *directions* '(:right :down :left :up))

(defun parse-map (lines)
  (let ((w (reduce #'max (mapcar #'length lines)))
        (h (length lines)))
    (loop for line in lines
          for y = 0 then (1+ y)
          with map = (make-array (list w h) :initial-element #\Space) 
          finally (return map) do
            (loop for c across line
                  for x = 0 then (1+ x) do
                    (setf (aref map x y) c)))))

(defun parse-path (s)
  (let ((ws (str:words (str:replace-using (list "L" " L " "R" " R ") s))))
    (loop for w in ws
          collect (or (parse-integer w :junk-allowed t)
                      (char w 0)))))

(defun parse-input (lines)
  (destructuring-bind (map path) (split-sequence-if #'str:emptyp lines)
    (list (parse-map map)
          (parse-path (first path)))))

(defun dir-int (d) (position d *directions*))
(defun int-dir (n) (nth n *directions*))
(defun reverse-dir (d) (int-dir (mod (+ 2 (dir-int d)) 4)))

(defun turn-left (pos)
  (destructuring-bind (x y d) pos
    (list x y (int-dir (mod (1- (dir-int d)) 4)))))

(defun turn-right (pos)
  (destructuring-bind (x y d) pos
    (list x y (int-dir (mod (1+ (dir-int d)) 4)))))

(defun get-delta (d)
  (ecase d
    (:right '(1 0))
    (:down '(0 1))
    (:left '(-1 0))
    (:up '(0 -1))))

(defun move-forward-v1 (map pos n)
  (destructuring-bind (x y d) pos
    (destructuring-bind (dx dy) (get-delta d)
      (destructuring-bind (w h) (array-dimensions map)
        (loop with tx = x with ty = y with i = 0 while (< i n) do
          (setf tx (mod (+ tx dx) w) ty (mod (+ ty dy) h))
          (case (aref map tx ty)
            (#\# ;; Hit a wall - stop moving
             (return)) 
            (#\. ;; Empty space - increase move count & update pos
             (incf i)
             (setf x tx y ty))))
        (list x y d)))))

(defun move-forward-v2 (map pos-map pos n)
  (destructuring-bind (x y d) pos
    (loop repeat n do
      (destructuring-bind (dx dy) (get-delta d)
        (let ((tx (+ x dx)) (ty (+ y dy)) (td d))
          (when-let (p (gethash (list tx ty d) pos-map))
            (setf tx (nth 0 p) ty (nth 1 p) td (nth 2 p)))
          (ecase (aref map tx ty)
            (#\. (setf x tx y ty d td))
            (#\# (return))))))
    (list x y d)))

(defun calc-password (pos)
  (destructuring-bind (x y d) pos
    (+ (* (1+ y) 1000)
       (* (1+ x) 4)
       (dir-int d))))

(defun test-map-p (map) (= (array-dimension map 0) 16))
(defun edge-length (map) (if (test-map-p map) 4 50))

(defun square-pos (map square)
  (nth (1- square)
       (if (test-map-p map)
           *test-squares*
           *squares*)))

(defun inside-edge-coords (map square edge)
  (let ((e (1- (edge-length map))))
    (destructuring-bind (x y) (square-pos map square)
      (ecase edge
        (:up (loop for i from 0 to e collect (list (+ x i) y)))
        (:down (loop for i from 0 to e collect (list (+ x i) (+ y e))))
        (:left (loop for i from 0 to e collect (list x (+ y i))))
        (:right (loop for i from 0 to e collect (list (+ x e) (+ y i))))))))

(defun outside-edge-coords (map square edge)
  (destructuring-bind (dx dy) (get-delta edge)
    (loop for (x y) in (inside-edge-coords map square edge)
          collect (list (+ x dx) (+ y dy)))))

(defun edge-mappings (map s1 e1 s2 e2 f)
  (loop for (x1 y1) in (outside-edge-coords map s1 e1)
        for (x2 y2) in (funcall f (inside-edge-coords map s2 e2))
        collect (cons (list x1 y1 e1)
                      (list x2 y2 (reverse-dir e2)))))

(defun all-mappings (map)
  (let ((c (if (test-map-p map) *test-connections* *connections*)))
    (loop for (s1 e1 s2 e2 f) in c
          append (edge-mappings map s1 e1 s2 e2 f)
          append (edge-mappings map s2 e2 s1 e1 f))))

(defun start-pos (map)
  (destructuring-bind (x y) (square-pos map 1)
    (list x y :right)))

(defun part1 (input)
  (destructuring-bind (map path) input
    (let ((move (lambda (pos cmd)
                  (cond
                    ((eql cmd #\L) (turn-left pos))
                    ((eql cmd #\R) (turn-right pos))
                    (t (move-forward-v1 map pos cmd))))))
      (calc-password (reduce move path :initial-value (start-pos map))))))

(defun part2 (input)
  (destructuring-bind (map path) input
    (let* ((pos-map (alist-hash-table (all-mappings map) :test #'equal))
           (move (lambda (pos cmd)
                           (cond
                             ((eql cmd #\L) (turn-left pos))
                             ((eql cmd #\R) (turn-right pos))
                             (t (move-forward-v2 map pos-map pos cmd))))))
      (calc-password (reduce move path :initial-value (start-pos map))))))

