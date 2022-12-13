(defpackage :day13
  (:use :cl :split-sequence)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day13)

(defun parse-list (s)
  (let ((*read-eval* nil))
    (read-from-string (str:replace-using '("[" "(" "]" ")" "," " ") s))))

(defun parse-pair (pair)
  (mapcar #'parse-list pair))

(defun parse-input (lines)
  (->> lines
    (split-sequence-if #'str:emptyp)
    (mapcar #'parse-pair)))

(defun cmp-lists (xs ys)
  (loop for x in xs
        for y in ys do
          (let ((c (cmp x y)))
            (unless (zerop c)
              (return-from cmp-lists c))))
  (- (length xs) (length ys)))

(defun cmp (a b)
  (cond
    ((and (listp a) (listp b)) (cmp-lists a b))
    ((listp a) (cmp-lists a (list b)))
    ((listp b) (cmp-lists (list a) b))
    (t (- a b))))

(defun part1 (pairs)
  (loop for p in pairs
        for i = 1 then (1+ i)
        when (minusp (apply #'cmp p))
          sum i))

(defun part2 (pairs)
  (let* ((d1 '((2)))
         (d2 '((6)))
         (lists (apply #'append (list d1 d2) pairs))
         (sorted (sort lists (lambda (a b) (minusp (cmp a b))))))  
    (* (1+ (position d1 sorted))
       (1+ (position d2 sorted)))))

