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
               (:file "day4")))
