(defpackage :day16
  (:use :cl :alexandria :org.tfeb.hax.memoize)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day16)

(defun parse-node (s)
  (let ((ws (str:words (str:replace-using '("=" " " ";" "" "," "") s))))
    (list (nth 1 ws)
          (parse-integer (nth 5 ws))
          (subseq ws 10))))

(defun parse-input (lines)
  (mapcar #'parse-node lines))

(defun neighbours (nodes id)
  (third (find-if (lambda (n) (equal (first n) id)) nodes)))

(defun flow-for-id (nodes id)
  (second (find-if (lambda (n) (equal (first n) id)) nodes)))

;; Djikstra to find shortest path between two nodes
(defun calc-distance (nodes src dst)
  (let ((ht (make-hash-table))
        (q (make-instance 'cl-heap:priority-queue))
        (cost-closed -1))
    (cl-heap:enqueue q src 0)
    (setf (gethash src ht) 0)
    (loop
      (let* ((pos (cl-heap:dequeue q))
             (cost (gethash pos ht)))
        (when (equal pos dst)
          (return cost))
        (unless (eql cost cost-closed)
          (loop for n in (neighbours nodes pos) do
            (let ((prev-cost (gethash n ht))
                  (new-cost (1+ cost)))
              (when (or (null prev-cost) (< new-cost prev-cost))
                (setf (gethash n ht) new-cost)
                (cl-heap:enqueue q n new-cost))))
          (setf (gethash pos ht) cost-closed))))))

(defun valve-ids (nodes)
  (->> nodes
    (remove-if (lambda (n) (zerop (second n))))
    (mapcar #'first)))

(defstruct valve
  (id "" :type string)
  (flow 0 :type fixnum)
  (costs nil :type (simple-array fixnum)))

;; Build a list of all the valve nodes, with precalculated
;; costs from each node to all other nodes.
(defun make-valves (nodes)
  (let ((ids (cons "AA" (valve-ids nodes))))
    (loop for src in ids
          collect
          (let ((costs (loop for dst in ids
                             collect (calc-distance nodes src dst))))
            (make-valve :id src
                        :flow (flow-for-id nodes src)
                        :costs (make-array (length costs)
                                           :element-type 'fixnum
                                           :initial-contents costs))))))

;; Generate an integer memoization key, allowing 16 bits for each argument.
;; This is more than enough. The visited bitmask needs 16 bits because there
;; are 16 valves, and the others all need less.
(defun memo-key (args)
  (destructuring-bind (valves visited index mins-remaining elephants) args
    (declare (ignore valves))
    (logior visited (ash index 16) (ash mins-remaining 32) (ash elephants 48))))

(def-memoized-function (calc-pressure :key #'memo-key) (valves visited index mins-remaining elephants)
  (let ((costs (valve-costs (aref valves index))))
    (let ((max-pressure 0))
      (loop for v across valves
            for i = 0 then (1+ i)
            for mask = 1 then (ash mask 1)
            unless (logtest visited mask) do
              (let* ((new-mins (- mins-remaining (aref costs i) 1))
                     (pressure (* new-mins (valve-flow v)))) 
                (when (>= new-mins 0)
                  (maxf max-pressure (+ pressure (calc-pressure valves (logior visited mask) i new-mins elephants))))))
      (when (plusp elephants)
        (maxf max-pressure (calc-pressure valves visited 0 26 (1- elephants))))
      max-pressure)))

(defun run (nodes mins elephants)
  (let ((valves (coerce (make-valves nodes) 'vector)))
    (calc-pressure valves 1 0 mins elephants)))

(defun part1 (nodes) (run nodes 30 0))
(defun part2 (nodes) (run nodes 26 1))

