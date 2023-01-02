(defpackage :day24
  (:use :cl)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day24)

(defun transpose (m)
  (apply #'mapcar 'list m))

(defun parse-input (lines)
  (let ((w (length (first lines)))
        (h (length lines))
        (data (mapcar (lambda (s) (coerce s 'list)) lines)))
    (make-array (list w h) :initial-contents (transpose data))))

(defun goal (map)
  (destructuring-bind (w h) (array-dimensions map)
    (list (- w 2) (- h 1))))

(defun walls (map)
  (destructuring-bind (w h) (array-dimensions map)
    (loop for x from 0 below w
          append (loop for y from 0 below h
                       for c = (aref map x y)
                       when (eql c #\#)
                         collect (list x y)))))

(defun blizzards (map)
  (destructuring-bind (w h) (array-dimensions map)
    (loop for x from 1 to (- w 2)
          append (loop for y from 1 to (- h 2)
                       for c = (aref map x y)
                       unless (eql c #\.)
                         collect (list c x y)))))

(defun move-blizzards (width height bs)
  (loop with inner-width = (- width 2)
        with inner-height = (- height 2)
        for (c x y) in bs
        for dx = (case c (#\> 1) (#\< -1) (otherwise 0))
        for dy = (case c (#\v 1) (#\^ -1) (otherwise 0))
        collect (list c (1+ (mod (+ x dx -1) inner-width))
                      (1+ (mod (+ y dy -1) inner-height)))))

;; Build a 3d map, with time as the third dimension
(defun make-tmap (map duration)
  (destructuring-bind (w h) (array-dimensions map)
    (loop with tmap = (make-array (list w h duration) :initial-element #\.)
          with walls = (walls map)
          for bs = (blizzards map) then (move-blizzards w h bs)
          for time from 0 below duration
          finally (return tmap) do
            (loop for (x y) in walls do
              (setf (aref tmap x y time) #\#))
            (loop for (c x y) in bs do
              (setf (aref tmap x y time) c)))))

;; Is the map clear at the given pos (x, y, time)?
(defun clearp (tmap pos)
  (and (notany #'minusp pos)
       (every #'< pos (array-dimensions tmap))
       (eql (apply #'aref tmap pos) #\.)))

;; Find all valid neighbours (i.e. left, right, down, up, or wait)
(defun neighbours (tmap pos)
  (loop for delta in '((1 0 1) (-1 0 1) (0 1 1) (0 -1 1) (0 0 1))
        for npos = (mapcar #'+ pos delta)
        when (clearp tmap npos)
          collect npos))

;; Cost for a position is the time so far plus the best possible
;; time to reach the goal (i.e. manhattan distance from pos to goal)
(defun calc-cost (pos goal)
  (destructuring-bind (x y time) pos
    (destructuring-bind (gx gy) goal
      (+ time (abs (- x gx)) (abs (- y gy))))))

(defun find-path (tmap src dst start-time)
  (let ((ht (make-hash-table :test #'equal))
        (q (make-instance 'cl-heap:priority-queue))
        (start-pos (append src (list start-time))))
    (cl-heap:enqueue q start-pos (calc-cost start-pos dst))
    (loop
      (let ((pos (cl-heap:dequeue q)))
        (destructuring-bind (x y time) pos
          (when (equal (list x y) dst)
            (return time)))
        (loop for n in (neighbours tmap pos) do
          (unless (gethash n ht)
            (cl-heap:enqueue q n (calc-cost n dst))
            (setf (gethash n ht) t)))))))

(defun part1 (map)
  (let ((tmap (make-tmap map 1000))
        (src '(1 0))
        (dst (goal map)))
    (find-path tmap src dst 0)))

(defun part2 (map)
  (let ((tmap (make-tmap map 1000))
        (src '(1 0))
        (dst (goal map)))
    (->> 0
      (find-path tmap src dst)
      (find-path tmap dst src)
      (find-path tmap src dst))))
