(defpackage :day15
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day15)

(defun get-elements (is lst)
  (mapcar (lambda (i) (nth i lst)) is))

(defun parse-sensor (line)
  (->> line
    (str:replace-using '("x=" "" "y=" "" ":" "" "," ""))
    (str:words)
    (get-elements '(2 3 8 9))
    (mapcar #'parse-integer)))

(defun parse-input (lines)
  (mapcar #'parse-sensor lines))

(defun manhattan-distance (ax ay bx by)
  (+ (abs (- ax bx))
     (abs (- ay by))))

(defun is-test (sensors) (= (length sensors) 14))

(defun exclusion-range (sensor row)
  (destructuring-bind (sx sy bx by) sensor
    (let ((w (- (manhattan-distance sx sy bx by) (abs (- row sy)))))
      (when (>= w 0)
        (list (- sx w) (+ sx w))))))

(defun exclusion-ranges (sensors row)
  (->> sensors
    (mapcar (lambda (s) (exclusion-range s row)))
    (remove nil)))

(defun is-point-near-sensor (s x y)
  (destructuring-bind (sx sy bx by) s
    (let ((distance-to-beacon (+ (abs (- sx bx)) (abs (- sy by))))
          (distance-to-point (+ (abs (- sx x)) (abs (- sy y)))))
      (<= distance-to-point distance-to-beacon))))

(defun is-rectangle-near-sensor (s min-x min-y max-x max-y)
  (and (is-point-near-sensor s min-x min-y)
       (is-point-near-sensor s min-x max-y)
       (is-point-near-sensor s max-x min-y)
       (is-point-near-sensor s max-x max-y)))

(defun find-gap (sensors min-x min-y max-x max-y)
  
  ;; Check each sensor. If the entire rectangle is within range
  ;; of this sensor, there's no gap.
  
  (loop for s in sensors do
    (when (is-rectangle-near-sensor s min-x min-y max-x max-y)
      (return-from find-gap nil)))

  ;; If it's a 1x1 rectangle and it's not near any sensor,
  ;; we've found the gap

  (when (and (= min-x max-x) (= min-y max-y))
    (return-from find-gap (list min-x min-y)))

  ;; Otherwise, split the rectangle in half and recurse.

  (let ((width (1+ (- max-x min-x)))
        (height (1+ (- max-y min-y))))
    (if (> width height)
        (let ((mid-x (+ min-x (floor width 2))))
          (or (find-gap sensors min-x min-y (1- mid-x) max-y)
              (find-gap sensors mid-x min-y max-x max-y)))
        (let ((mid-y (+ min-y (floor height 2))))
          (or (find-gap sensors min-x min-y max-x (1- mid-y))
              (find-gap sensors min-x mid-y max-x max-y))))))

(defun count-beacons-on-row (sensors row)
  (->> sensors
    (mapcar (lambda (s) (subseq s 2)))
    (lambda (beacons) (remove-duplicates beacons :test #'equal))
    (count-if (lambda (s) (= (second s) row)))))

(defun ranges-size (ranges)
  ;; Assuming here that there are no gaps
  (let ((min (apply #'min (mapcar #'first ranges)))
        (max (apply #'max (mapcar #'second ranges))))
    (1+ (- max min))))

(defun part1 (sensors)
  (let ((row (if (is-test sensors) 10 2000000)))
    (- (ranges-size (exclusion-ranges sensors row))
       (count-beacons-on-row sensors row))))

(defun part2 (sensors)
  (let ((max-range (if (is-test sensors) 20 4000000)))
    (destructuring-bind (x y) (find-gap sensors 0 0 max-range max-range)
      (+ (* x 4000000) y))))

