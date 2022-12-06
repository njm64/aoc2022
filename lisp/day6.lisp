(defpackage :day6
  (:use :cl))

(in-package :day6)

(defun parse-input (lines) (coerce (first lines) 'list))

(defun find-marker (buf len)
  (loop for s on buf
        for n = 0 then (1+ n)
        until (= len (length (remove-duplicates (subseq s 0 len))))
        finally (return (+ n len))))

(defun part1 (s) (find-marker s 4))
(defun part2 (s) (find-marker s 14))
