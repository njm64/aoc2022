(require 'aoc)

(setq aoc-day 7)

(defun node-name (node) (aref node 2))
(defun node-type (node) (aref node 3))
(defun node-size (node) (aref node 4))
(defun node-children (node) (aref node 5))

(defun add-child (node child)
  (push child (aref node 5)))

(defun make-dir (name)
  (record 'node 4 name :dir 0 nil))

(defun make-file (name size)
  (record 'node 4 name :file size nil))

(defun find-child (node name)
  (seq-find (lambda (c) (equal (node-name c) name))
            (node-children node)))

(defun aoc-parse (lines)
  (let* ((root (make-dir "/"))
         (cwd root)
         (stack nil))
    (dolist (line lines)
      (pcase (words line)
        (`("$" "ls"))
        (`("$" "cd" "/")
         (setf cwd root stack nil))
        (`("$" "cd" "..")
         (setf cwd (pop stack)))
        (`("$" "cd" ,name)
         (push cwd stack)
         (setf cwd (find-child cwd name)))
        (`("dir" ,name)
         (add-child cwd (make-dir name)))
        (`(,size ,name)
         (add-child cwd (make-file name (string-to-number size))))))
    root))

(defun all-dirs (node)
  (when (eql (node-type node) :dir)
    (cons node (seq-mapcat #'all-dirs (node-children node)))))

(defun total-size (node)
  (+ (node-size node)
     (sum (mapcar #'total-size (node-children node)))))

(defun aoc-part1 (root)
  (thread-last (all-dirs root)
               (mapcar #'total-size)
               (seq-filter (lambda (s) (<= s 100000)))
               (sum)))

(defun aoc-part2 (root)
  (let* ((free-space (- 70000000 (total-size root)))
         (required-space (- 30000000 free-space)))
    (thread-last (all-dirs root)
                 (mapcar #'total-size)
                 (seq-filter (lambda (s) (>= s required-space)))
                 (seq-min))))


