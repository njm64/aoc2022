(defpackage :day19
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day19)

;; A blueprint is a vector of robots (in order for resources),
;; where each robot is a list of the resources required
(defun parse-blueprint (line)
  (let ((ws (str:words (str:replace-using '(":" "" "." "") line))))
    (list (vector (parse-integer (nth 6 ws)) 0 0 0)
          (vector (parse-integer (nth 12 ws)) 0 0 0)
          (vector (parse-integer (nth 18 ws)) (parse-integer (nth 21 ws)) 0 0)
          (vector (parse-integer (nth 27 ws)) 0 (parse-integer (nth 30 ws)) 0))))

;; Calculate the maximum of each resource that is required for robot
;; construction. Since we can only calculate one robot per turn, we won't
;; need more than this many robots for each resource type.
(defun calc-robot-limits (bp)
  (let ((limits (reduce (lambda (a b) (map 'vector #'max a b)) bp)))
    (setf (aref limits 3) 1000)
    limits))

(defun parse-input (lines)
  (mapcar #'parse-blueprint lines))

(defclass state ()
  ((time :initarg :time)
   (robots :initarg :robots)
   (resources :initarg :resources)))

(defun make-initial-state ()
  (make-instance 'state
                 :time 1
                 :robots #(1 0 0 0)
                 :resources #(0 0 0 0)))

(defun clone-state (s)
  (with-slots (time robots resources) s
      (make-instance 'state
                     :time time
                     :robots (copy-array robots)
                     :resources (copy-array resources))))

;; Check there's at least one robot for each of the
;; resources in the resource array.
(defun has-robots (s resources)
  (every (lambda (current required)
           (or (zerop required) (plusp current)))
         (slot-value s 'robots)
         resources))

(defun add-resources (ra rb) (map-into ra #'+ ra rb))
(defun sub-resources (ra rb) (map-into ra #'- ra rb))

(defun count-geodes (s at-time)
  (with-slots (resources robots time) s
    (let ((remaining-mins (1+ (- at-time time))))
      (+ (aref resources 3)
         (* (aref robots 3) remaining-mins)))))

(defun run-blueprint (bp max-time)
  (let ((stack (list (make-initial-state)))
        (max-geodes 0)
        (limits (calc-robot-limits bp)))
    (loop for s = (pop stack) while s do
      (maxf max-geodes (count-geodes s max-time))
      (loop for robot-resources in bp
            for robot-type = 0 then (1+ robot-type) do
              (when (and (< (aref (slot-value s 'robots) robot-type)
                            (aref limits robot-type))
                         (has-robots s robot-resources))
                (let ((new-state (clone-state s)))
                  (with-slots (time robots resources) new-state
                    ;; Mine until we have enough resources or we're out of time
                    (loop until (every #'>= resources robot-resources) 
                          while (< time max-time) do
                            (add-resources resources robots)
                            (incf time))
                    (if (= time max-time)
                        ;; If we're out of time, just count the maximum number for this state
                        (maxf max-geodes (count-geodes s max-time))
                        ;; Otherwise we have enough resources, so build the new robot
                        (progn
                          (sub-resources resources robot-resources)
                          (add-resources resources robots)
                          (incf (aref robots robot-type))
                          (incf time)
                          (push new-state stack))))))))
    max-geodes))

(defun part1 (blueprints)
  (loop for bp in blueprints
        for i = 1 then (1+ i)
        sum (* i (run-blueprint bp 24))))

(defun part2 (blueprints)
  (->> (subseq blueprints 0 3)
    (mapcar (lambda (bp) (run-blueprint bp 32)))
    (apply #'*)))
