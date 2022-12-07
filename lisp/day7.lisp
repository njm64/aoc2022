(defpackage :day7
  (:use :cl :uiop :trivia)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day7)

(defclass node ()
    ((name :initarg :name :accessor node-name)
     (type :initarg :type :accessor node-type)
     (size :initarg :size :accessor node-size :initform 0)
     (children :accessor node-children :initform nil)))

(defun find-child (node name)
  (find-if (lambda (c) (equal (node-name c) name))
           (node-children node)))

(defun add-child (node child)
  (unless (find-child node (node-name child))
    (push child (node-children node))))

(defun make-dir (name)
  (make-instance 'node :type :dir :name name))

(defun make-file (name size)
  (make-instance 'node :type :file :name name :size size))

(defun print-node (node &optional (depth 0))
  (with-slots (name type size children) node
    (loop repeat depth do (format t " "))
    (format t "Name: ~a Type: ~a Size: ~a~%" name type size)
    (loop for child in children do
      (print-node child (1+ depth)))))

(defun parse-input (lines)
  (let* ((root (make-dir "/"))
         (cwd root)
         (stack nil))
    (loop for line in lines do
      (match (str:words line)
        ((list "$" "ls"))
        ((list "$" "cd" "/")
         (setf cwd root stack nil))
        ((list "$" "cd" "..")
         (setf cwd (pop stack)))
        ((list "$" "cd" name)
         (push cwd stack)
         (setf cwd (find-child cwd name)))
        ((list "dir" name)
         (add-child cwd (make-dir name)))
        ((list size name)
         (add-child cwd (make-file name (parse-integer size))))))
    root))

(defun all-dirs (node)
  (when (eql (node-type node) :dir)
    (cons node (loop for n in (node-children node)
                     append (all-dirs n)))))

(defun total-size (node)
  (+ (node-size node)
     (loop for n in (node-children node)
           sum (total-size n))))

(defun part1 (root)
  (->> (all-dirs root)
    (mapcar #'total-size)
    (remove-if (lambda (s) (> s 100000)))
    (reduce #'+)))

(defun part2 (root)
  (let* ((free-space (- 70000000 (total-size root)))
         (required-space (- 30000000 free-space)))
    (->> (all-dirs root)
      (mapcar #'total-size)
      (remove-if (lambda (s) (< s required-space)))
      (reduce #'min))))
