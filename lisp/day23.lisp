(defpackage :day23
  (:use :cl :alexandria))

(in-package :day23)

(defparameter *directions* '(:north :south :west :east))

(defun parse-input (lines)
  (loop for line in lines
        for y = 0 then (1+ y)
        append (loop for c across line
                     for x = 0 then (1+ x)
                     when (eql c #\#)
                       collect (cons x y))))

(defun rotate-directions (directions)
  (append (rest directions) (list (first directions))))

(defun move (p direction)
  (destructuring-bind (x . y) p
    (ecase direction
      (:north (cons x (1- y)))
      (:south (cons x (1+ y)))
      (:east  (cons (1+ x) y))
      (:west  (cons (1- x) y)))))

(defun diagonals (p direction)
  (destructuring-bind (x . y) p
    (ecase direction
      (:north (list (cons (1- x) (1- y)) (cons (1+ x) (1- y))))
      (:south (list (cons (1- x) (1+ y)) (cons (1+ x) (1+ y))))
      (:east  (list (cons (1+ x) (1- y)) (cons (1+ x) (1+ y))))
      (:west  (list (cons (1- x) (1- y)) (cons (1- x) (1+ y)))))))

(defun clearp (ht p direction)
  (notany (lambda (p) (gethash p ht))
          (cons (move p direction) (diagonals p direction))))

(defun isolated (ht elf)
  (and (clearp ht elf :north)
       (clearp ht elf :south)
       (clearp ht elf :east)
       (clearp ht elf :west)))

(defun propose-move (ht directions elf)
  (unless (isolated ht elf)
    (when-let (d (find-if (lambda (d) (clearp ht elf d)) directions))
      (move elf d))))

(defun make-elf-table (elves)
  (loop with et = (make-hash-table :test #'equal)
        finally (return et)
        for e in elves do
          (setf (gethash e et) t)))

(defun move-elves (elves directions)
  (let* ((et (make-elf-table elves))
         (moves (loop for e in elves collect (propose-move et directions e))))

    (loop for m in moves when m do
      (incf (gethash m et 0)))

    (loop for e in elves
          for m in moves
          for valid = (and m (= (gethash m et) 1))
          collect (if valid m e) into ret
          count valid into valid-count
          finally (return (values ret valid-count)))))

(defun move-elves-v2 (elves directions)
  (multiple-value-bind (new-elves move-count) (move-elves elves directions)
    (unless (zerop move-count)
      new-elves)))

(defun count-empty (elves)
  (loop for (x . y) in elves
        minimizing x into x1
        maximizing x into x2
        minimizing y into y1
        maximizing y into y2
        finally (let ((w (1+ (- x2 x1)))
                      (h (1+ (- y2 y1))))
                  (return (- (* h w) (length elves))))))

(defun part1 (elves)
  (loop for es = elves then (move-elves es ds)
        for ds = *directions* then (rotate-directions ds)
        repeat 10
        finally (return (count-empty es))))

(defun part2 (elves)
  (loop for es = elves then (move-elves-v2 es ds)
        for ds = *directions* then (rotate-directions ds)
        while es 
        for i = 1 then (1+ i)
        finally (return i)))


