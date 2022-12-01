(defpackage :aoc
  (:use :cl)
  (:export :run :test :read-input :read-raw))

(in-package :aoc)

(defparameter *input-dir* "../input")

(defun opt-package-for-day (d)
  (find-package (format nil "DAY~d" d)))

(defun package-for-day (d)
  (or (opt-package-for-day d)
      (error "Not implemented")))

(defun read-raw (d)
  (let ((filename (format nil "~a/day~d.txt" *input-dir* d)))
    (uiop:read-file-lines filename)))

(defun read-input (d)
  (let* ((package (package-for-day d))
         (parse-input (or (find-symbol "PARSE-INPUT" package)
                          (error "Missing parse-input"))))
    (funcall parse-input (read-raw d))))

(defun run-part (d p f input)
  (let* ((t1 (get-internal-run-time))
         (result (funcall f input))
         (t2 (get-internal-run-time))
         (elapsed (/ (- t2 t1) internal-time-units-per-second)))
    (format t "Day ~2d Part ~d: ~20@<~d~> ~,6fs~%" d p result elapsed)))

(defun run (&optional d)
  (if d
      ;; If a day is specified, run it
      (let* ((package (package-for-day d))
             (input (read-input d))
             (part1 (find-symbol "PART1" package))
             (part2 (find-symbol "PART2" package)))
        (when part1 (run-part d 1 part1 input))
        (when part2 (run-part d 2 part2 input)))
      ;; Otherwise run all days
      (loop for d from 1 to 25 do
        (when (opt-package-for-day d)
          (run d)))))

(defun test (enable)
  (if enable
      (setf *input-dir* "../test")
      (setf *input-dir* "../input"))
  enable)



