(defpackage :day21
  (:use :cl :alexandria))

(in-package :day21)

;; Monkeys are represented as a list of 2 or 4 elements:
;; (id, integer) or (id, a, operand, b)
(defun parse-monkey (s)
  (destructuring-bind (id value) (str:split ": " s)
    (let ((tok (str:words value)))
      (cons (intern (string-upcase id))
            (ecase (length tok)
              (1 (list (parse-integer (first tok))))
              (3 (list (intern (string-upcase (first tok)))
                       (char (second tok) 0)
                       (intern (string-upcase (third tok))))))))))

(defun parse-input (lines) (mapcar #'parse-monkey lines))

(defun resolve-fn (f)
  (ecase f
    (#\+ #'+)
    (#\- #'-)
    (#\* #'*)
    (#\/ #'floor)))

;; Find a monkey that references the given id, excluding the last-monkey
(defun find-monkey (monkeys id last-monkey)
  (find-if (lambda (m) (and (find id m) (not (eq m last-monkey))))
           monkeys))

;; Solve a monkey expression for the given id
(defun solve (monkey id)
  (destructuring-bind (a b op c) monkey
    (cond
      ((eq id a) (list b op c))              ;; a = b + c
      ((eq id b) (ecase op
                   (#\+ (list a #\- c))      ;; a = b + c -> b = a - c
                   (#\- (list a #\+ c))      ;; a = b - c -> b = a + c
                   (#\* (list a #\/ c))      ;; a = b * c -> b = a / c
                   (#\/ (list a #\* c))))    ;; a = b / c -> b = a * c
      ((eq id c) (ecase op
                   (#\+ (list a #\- b))      ;; a = b + c -> c = a - b
                   (#\- (list b #\- a))      ;; a = b - c -> c = b - a
                   (#\* (list a #\/ b))      ;; a = b * c -> c = a / b
                   (#\/ (list b #\/ a))))))) ;; a = b / c -> c = b / a

(defun resolve-value (monkeys id last-monkey)
  (let* ((monkey (find-monkey monkeys id last-monkey)))
    (if (= (length monkey) 2)
        ;; If it's an integer value, just return it
        (second monkey)
        ;; Otherwise it's an binary function. Solve for id,
        ;; recursively resolve operands, and call the function.
        (destructuring-bind (a f b) (solve monkey id)
          (funcall (resolve-fn f)
                   (resolve-value monkeys a monkey)
                   (resolve-value monkeys b monkey))))))

(defun part1 (monkeys)
  (resolve-value monkeys 'root nil))

(defun part2 (monkeys)
  (let ((root (find-if (lambda (m) (eq (first m) 'root)) monkeys))
        (humn (find-if (lambda (m) (eq (first m) 'humn)) monkeys)))
    ;; Remove the root and humn monkeys from the list
    (removef monkeys root)
    (removef monkeys humn)
    ;; Transform the root expression, effectively saying
    ;; that the first operand is equal to the value of the second.
    (let ((monkey (list (nth 1 root) (resolve-value monkeys (nth 3 root) nil))))
      (push monkey monkeys))
    ;; Now just resolve the value for humn
    (resolve-value monkeys 'humn nil)))


