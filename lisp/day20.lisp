(defpackage :day20
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day20)

;; Our implementation is a doubly-linked circular list
(defclass node ()
  ((value :initarg :value :accessor node-value)
   (prev :initarg :prev :accessor node-prev)
   (next :initarg :next :accessor node-next)))

(defun parse-input (lines)
  (mapcar #'parse-integer lines))

(defun make-node (value)
  (make-instance 'node :value value))

(defun link-nodes (a b)
  (setf (slot-value a 'next) b
        (slot-value b 'prev) a))

(defun make-node-list (numbers)
  (let ((nodes (mapcar #'make-node numbers)))
    (loop for curr in nodes
          and prev = curr
          when prev do (link-nodes prev curr))
    (link-nodes (lastcar nodes) (first nodes))
    nodes))

(defun scan-forward (node n)
  (loop repeat n do (setf node (node-next node)))
  node)

(defun scan-backward (node n)
  (loop repeat n do (setf node (node-prev node)))
  node)

;; For part 2 we need to take the mod of the list length
;; when calculating the scan distance. The exception to
;; this rule is when the mod is zero. We're already removed
;; the soure node from the list, so a scan distance of zero
;; will do nothing. Use the list length instead.
(defun wrap (n length)
  (let ((r (mod n length)))
    (if (zerop r)
        length
        r)))

(defun scan (node list-length)
  (with-slots (value) node
    (if (plusp value)
        (scan-forward node (wrap value list-length))
        (scan-backward node (wrap (1+ (- value)) list-length)))))

(defun find-zero (nodes)
  (find-if (lambda (node) (zerop (node-value node))) nodes))

(defun remove-node (node)
  (with-slots (prev next) node
    (setf (node-next prev) next
          (node-prev next) prev)))

(defun insert-node (node prev)
  (let ((next (node-next prev)))
    (setf (node-next prev) node
          (node-prev next) node
          (node-prev node) prev
          (node-next node) next)))

(defun mix (node list-length)
  ;; It's important to remove the node before scanning,
  ;; to get the correct result in cases where the node value
  ;; is longer than the length of the list. Also subtract one
  ;; from the list length below for the same reason.
  (remove-node node)
  (let ((prev (scan node (1- list-length))))
    (insert-node node prev)))

(defun run (mix-count nodes)
  (let ((list-length (length nodes)))
    (loop repeat mix-count do
      (loop for node in nodes do
        (mix node list-length)))
    (let ((z (find-zero nodes)))
      (+ (node-value (scan-forward z 1000))
         (node-value (scan-forward z 2000))
         (node-value (scan-forward z 3000))))))

(defun part1 (numbers)
  (run 1 (make-node-list numbers)))

(defun part2 (numbers)
  (->> numbers
    (mapcar (lambda (n) (* n 811589153)))
    (make-node-list)
    (run 10)))
