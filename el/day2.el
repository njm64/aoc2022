(require 'aoc)

(setq aoc-day 2)

(defun parse-line (line)
  (mapcar (lambda (s) (aref s 0))
          (words line)))

(defun aoc-parse (lines)
  (mapcar 'parse-line lines))

(defvar *rules*
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
  (pcase c
    (?A :rock)
    (?B :paper)
    (?C :scissors)
    (?X :rock)
    (?Y :paper)
    (?Z :scissors)))

(defun char->result (c)
  (pcase c
    (?X :lose)
    (?Y :draw)
    (?Z :win)))

(defun result->score (r)
  (pcase r
    (:lose 0)
    (:draw 3)
    (:win 6)))

(defun shape->score (s)
  (pcase s
    (:rock 1)
    (:paper 2)
    (:scissors 3)))

(defun find-result (turn)
  (let ((their-shape (char->shape (car turn)))
        (our-shape (char->shape (cadr turn))))
    (seq-some (lambda (r)
                (and (eq their-shape (car r))
                     (eq our-shape (cadr r))
                     (caddr r)))
              *rules*)))

(defun find-shape (turn)
  (let ((their-shape (char->shape (car turn)))
        (result (char->result (cadr turn))))
    (seq-some (lambda (r)
                (and (eq their-shape (car r))
                     (eq result (caddr r))
                     (cadr r)))
              *rules*)))

(defun score1 (turn)
  (let ((r (find-result turn))
        (s (char->shape (cadr turn))))
    (+ (result->score r) (shape->score s))))

(defun score2 (turn)
  (let ((s (find-shape turn))
        (r (char->result (cadr turn))))
    (+ (result->score r) (shape->score s))))

(defun aoc-part1 (turns) (sum (mapcar 'score1 turns)))
(defun aoc-part2 (turns) (sum (mapcar 'score2 turns)))

