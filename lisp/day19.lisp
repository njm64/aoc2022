 (defpackage :day19
     (:use :cl :alexandria :org.tfeb.hax.memoize)
     (:shadowing-import-from :arrow-macros :->>))

(in-package :day19)

(defstruct res
  (ore 0 :type fixnum)
  (clay 0 :type fixnum)
  (obsidian 0 :type fixnum)
  (geode 0 :type fixnum))

(defstruct state
  (time 1 :type fixnum)
  (robots (make-res :ore 1) :type res)
  (resources (make-res) :type res))

;; A blueprint is a vector of robots (in order for resources),
;; where each robot is a list of the resources required
(defun parse-blueprint (line)
  (let ((ws (str:words (str:replace-using '(":" "" "." "") line))))
    (list (make-res :ore (parse-integer (nth 6 ws)))
          (make-res :ore (parse-integer (nth 12 ws)))
          (make-res :ore (parse-integer (nth 18 ws)) :clay (parse-integer (nth 21 ws)))
          (make-res :ore (parse-integer (nth 27 ws)) :obsidian (parse-integer (nth 30 ws))))))

;; Calculate the maximum of each resource that is required for robot
;; construction. Since we can only calculate one robot per turn, we won't
;; need more than this many robots for each resource type.
(defun calc-robot-limits (bp)
  (loop for r in bp
        with limits = (make-res :geode 1000)
        finally (return limits) do
          (maxf (res-ore limits) (res-ore r))
          (maxf (res-clay limits) (res-clay r))
          (maxf (res-obsidian limits) (res-obsidian r))))

(defun parse-input (lines)
  (mapcar #'parse-blueprint lines))

;; Check there's at least one robot for each of the
;; resources in the resource array.
(defun has-robots (s resources)
  (let ((robots (state-robots s)))
    (and (or (zerop (res-ore resources)) (plusp (res-ore robots)))
         (or (zerop (res-clay resources)) (plusp (res-clay robots)))
         (or (zerop (res-obsidian resources)) (plusp (res-obsidian robots))))))

(defun add-resources (ra rb)
  (incf (res-ore ra) (res-ore rb))
  (incf (res-clay ra) (res-clay rb))
  (incf (res-obsidian ra) (res-obsidian rb))
  (incf (res-geode ra) (res-geode rb)))

(defun sub-resources (ra rb)
  (decf (res-ore ra) (res-ore rb))
  (decf (res-clay ra) (res-clay rb))
  (decf (res-obsidian ra) (res-obsidian rb))
  (decf (res-geode ra) (res-geode rb)))

(defun have-resources (ra rb)
  (and (>= (res-ore ra) (res-ore rb))
       (>= (res-clay ra) (res-clay rb))
       (>= (res-obsidian ra) (res-obsidian rb))))

(defun count-geodes (s at-time)
  (let ((remaining-mins (1+ (- at-time (state-time s)))))
    (+ (res-geode (state-resources s))
       (* (res-geode (state-robots s)) remaining-mins))))

(defun res-n (r n)
  (ecase n
    (0 (res-ore r))
    (1 (res-clay r))
    (2 (res-obsidian r))
    (3 (res-geode r))))

(defun inc-res-n (r n)
  (ecase n
    (0 (incf (res-ore r)))
    (1 (incf (res-clay r)))
    (2 (incf (res-obsidian r)))
    (3 (incf (res-geode r)))))

(def-memoized-function tri-sequence (n)
  (if (zerop n) 0 (+ n (tri-sequence (1- n)))))

;; This clever optimisation is from here:
;; https://www.reddit.com/r/adventofcode/comments/zpihwi/comment/j0tls7a/
(defun max-possible-geodes (s max-time)
  (let ((remaining-mins (1+ (- max-time (state-time s)))))
    (+ (res-geode (state-resources s))
       (* (res-geode (state-robots s)) remaining-mins)
       (tri-sequence remaining-mins))))

(defun run-blueprint (bp max-time)
  (loop with stack = (list (make-state))
        with limits = (calc-robot-limits bp)
        with max-geodes = 0
        finally (return max-geodes)
        for s = (pop stack) while s
        when (> (max-possible-geodes s max-time) max-geodes) do
          (maxf max-geodes (count-geodes s max-time))
          (loop for robot-resources in bp
                for robot-type = 0 then (1+ robot-type) do
                  (when (and (< (res-n (state-robots s) robot-type)
                                (res-n limits robot-type))
                             (has-robots s robot-resources))
                    (let ((time (state-time s))
                          (robots (copy-res (state-robots s)))
                          (resources (copy-res (state-resources s))))
                      (loop until (have-resources resources robot-resources) 
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
                            (inc-res-n robots robot-type)
                            (incf time)
                            (push (make-state :time time :robots robots :resources resources) stack))))))))

(defun part1 (blueprints)
  (loop for bp in blueprints
        for i = 1 then (1+ i)
        sum (* i (run-blueprint bp 24))))

(defun part2 (blueprints)
  (->> (subseq blueprints 0 3)
    (mapcar (lambda (bp) (run-blueprint bp 32)))
    (apply #'*)))
