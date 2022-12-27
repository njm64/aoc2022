(defsystem :aoc2022
  :author "Nick Maher"
  :description "Advent of Code 2022"
  :depends-on (:alexandria
               :str
               :split-sequence
               :trivia
               :arrow-macros
               :cl-ppcre
               :metabang-bind
               :cl-heap
               :memoize)
  :components ((:file "aoc")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day15")
               (:file "day16")
               (:file "day17")
               (:file "day18")
               (:file "day19")
               (:file "day20")))
