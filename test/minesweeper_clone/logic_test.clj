(ns minesweeper-clone.logic-test
  (:require [minesweeper-clone.logic :refer :all]
            [midje.sweet :refer :all]))

(fact "get-new-mine-position"
      (get-new-mine-position [2 2]
                             [1 1]
                             [[2 "M"]
                              [2 "M"]]
                             [["H" "H"]
                              [2 "H"]])
      => [[0 0]]

      (get-new-mine-position [3 3]
                             [2 2]
                             [[0 "M" "M"]
                              [0 0 0]
                              [0 0 "M"]]
                             [[1 "H" "H"]
                              ["H" 3 3]
                              ["H" "H" "H"]])
      => [[2 1]]
      )

; TODO: Need lots more tests
; Maybe time performance on large boards? Lots of mines? Few mines?