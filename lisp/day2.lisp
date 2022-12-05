(defpackage :day2
  (:use :cl))

(in-package :day2)

(defun parse-line (line) (mapcar (lambda (s) (char s 0)) (str:words line)))
(defun parse-input (lines) (mapcar #'parse-line lines))

(defparameter *rules*
  '((:rock :rock :draw)
    (:rock :paper :win)
    (:rock :scissors :lose)
    (:paper :rock :lose)
    (:paper :paper :draw)
    (:paper :scissors :win)
    (:scissors :rock :win)
    (:scissors :paper :lose)
    (:scissors :scissors :draw)))

(defun char->shape (c)
  (ecase c
    (#\A :rock)
    (#\B :paper)
    (#\C :scissors)
    (#\X :rock)
    (#\Y :paper)
    (#\Z :scissors)))

(defun char->result (c)
  (ecase c
    (#\X :lose)
    (#\Y :draw)
    (#\Z :win)))

(defun result->score (r)
  (ecase r
    (:lose 0)
    (:draw 3)
    (:win 6)))

(defun shape->score (p)
  (ecase p
    (:rock 1)
    (:paper 2)
    (:scissors 3)))

(defun find-result (turn)
  (let ((their-shape (char->shape (first turn)))
        (our-shape (char->shape (second turn))))
    (loop for (theirs ours r) in *rules*
          until (and (eql ours our-shape)
                     (eql theirs their-shape))
                finally (return r))))

(defun find-shape (turn)
  (let ((their-shape (char->shape (first turn)))
        (result (char->result (second turn))))
    (loop for (theirs ours r) in *rules*
          until (and (eql theirs their-shape)
                     (eql result r))
          finally (return ours))))

(defun score1 (turn)
  (let ((r (find-result turn))
        (s (char->shape (second turn))))
    (+ (result->score r) (shape->score s))))

(defun score2 (turn)
  (let ((s (find-shape turn))
        (r (char->result (second turn))))
    (+ (result->score r) (shape->score s))))

(defun part1 (turns) (reduce #'+ (mapcar #'score1 turns)))
(defun part2 (turns) (reduce #'+ (mapcar #'score2 turns)))
