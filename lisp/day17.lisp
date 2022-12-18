(defpackage :day17
  (:use :cl :alexandria))

(in-package :day17)

(defparameter *shapes*
  '#(#(#b0011110)
     #(#b0001000 #b0011100 #b0001000)
     #(#b0011100 #b0000100 #b0000100)
     #(#b0010000 #b0010000 #b0010000 #b0010000)
     #(#b0011000 #b0011000)))

(defun parse-input (lines)
  (first lines))

(defclass state ()
  ((rows :initarg :rows)
   (height :initarg :height :initform 0)
   (input :initarg :input)
   (next-input-index :initform 0)
   (next-shape-index :initform 0)))

(defun init-state (max-rows input)
  (make-instance 'state
                 :rows (make-array max-rows :initial-element 0)
                 :input input))

(defun place-shape (state shape x y)
  (with-slots (height rows) state
    (maxf height (+ y (length shape)))
    (loop for shape-row across shape
          for ry = y then (1+ ry) do
            (setf (aref rows ry) (logior (aref rows ry)
                                         (ash shape-row (- x)))))))

(defun can-place (state shape x y)
  ;; Check against the floor
  (when (minusp y)
    (return-from can-place nil))
  
  (loop with existing-rows = (slot-value state 'rows)
        for shape-row across shape
        for shifted-row = (ash shape-row (- x))
        for ry = y then (1+ ry)

        for existing-row = (elt existing-rows ry)
        finally (return t) do
          
          ;; Left and right walls. Just shift by the x position
          ;; and make sure the bit count hasn't changed.
          (unless (= (logcount (logand #x7f shifted-row))
                     (logcount shape-row))
            (return-from can-place nil))

          ;; Check against the existing row at this y position
          (unless (zerop (logand existing-row shifted-row))
            (return-from can-place nil))))

(defun next-shape (state)
  (with-slots ((i next-shape-index)) state
    (prog1 (elt *shapes* i)
      (setf i (mod (1+ i) (length *shapes*))))))

(defun next-input (state)
  (with-slots (input (i next-input-index)) state
    (prog1 (elt input i)
      (setf i (mod (1+ i) (length input))))))

(defun adjust-x (x d)
  (ecase d
    (#\< (1- x))
    (#\> (1+ x))))

(defun drop-shape (state)
  (with-slots (height) state
      (let ((shape (next-shape state))
            (x 0)
            (y (+ 3 height)))

        (loop for d = (next-input state) do
          
          ;; Apply horizontal movement
          (when (can-place state shape (adjust-x x d) y)
            (setf x (adjust-x x d)))

          ;; Try to apply vertical movement
          (if (can-place state shape x (1- y))
              (setf y (1- y))
              (return)))

        (place-shape state shape x y))))

(defun find-cycle (input)
  (let ((state (init-state 100000 input))
        (ht (make-hash-table :test #'equal)))
    (loop repeat (length input) do (drop-shape state))
    (with-slots (next-input-index next-shape-index height) state
      (loop for i = (length input) then (1+ i) do
        (let ((key (cons next-input-index next-shape-index)))
          (when-let (prev (gethash key ht))
            (return-from find-cycle (list prev (- i prev))))
          (setf (gethash key ht) i)
          (drop-shape state))))))

(defun part1 (input)
  (let ((state (init-state 500000 input)))
    (with-slots (height) state
      (loop repeat 2022 do (drop-shape state))
      height)))

(defun part2 (input)
  (destructuring-bind (cycle-start cycle-length) (find-cycle input)
    (let ((state (init-state 100000 input)))
      (with-slots (height) state
        (loop repeat cycle-start do (drop-shape state))
        (let ((height-before-cycle height))
          (loop repeat cycle-length do (drop-shape state))
          (let ((cycle-height (- height height-before-cycle)))
            (multiple-value-bind (cycle-count remainder)
                (floor (- 1000000000000 cycle-start) cycle-length)
              (loop repeat remainder do (drop-shape state))
              (+ height (* (1- cycle-count) cycle-height)))))))))


