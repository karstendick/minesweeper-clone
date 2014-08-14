(ns minesweeper-clone.core-test
  (:require [minesweeper-clone.core :refer :all]
            [midje.sweet :refer :all]))

(fact "in-bounds?"
      (let [[num-rows num-cols _] (:beginner board-defaults)]
        (in-bounds? 0 0 num-rows num-cols) => true
        (in-bounds? 1 1 num-rows num-cols) => true
        (in-bounds? -1 0 num-rows num-cols) => false
        (in-bounds? 0 -1 num-rows num-cols) => false
        (in-bounds? (inc num-rows) 0 num-rows num-cols) => false
        (in-bounds? 0 (inc num-cols) num-rows num-cols) => false))

(fact "count-neighbors"
      (let [num-rows 3
            num-cols 5
            board [[0 "M" "M" "M" 0]
                   [0 0 0 0 0]
                   [0 0 0 0 0]]]
        (count-neighbors board 0 0 num-rows num-cols) => 1
        (count-neighbors board 1 1 num-rows num-cols) => 2
        (count-neighbors board 1 2 num-rows num-cols) => 3))

(fact "vector->board"
      (let [num-rows 3
            num-cols 5
            v (range 15)
            board [(range 0 5)
                   (range 5 10)
                   (range 10 15)]]
        (vector->board v num-rows num-cols) => board))

(fact "calc-board"
      (let [[num-rows num-cols _] (:beginner board-defaults)
            board [["M" 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 "M" 0]
                   ["M" 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 "M" "M" 0]
                   ["M" 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 "M" 0 "M"]
                   [0 0 0 0 0 0 0 0 0]
                   [0 0 0 0 0 0 0 "M" 0]
                   [0 0 0 0 0 0 0 0 "M"]]
            calculated-board [["M" 1 0 0 0 0 1 1 1]
                              [2 2 0 0 0 0 1 "M" 1]
                              ["M" 1 0 0 0 1 3 3 2]
                              [2 2 0 0 0 1 "M" "M" 1]
                              ["M" 1 0 0 0 2 3 4 2]
                              [1 1 0 0 0 1 "M" 2 "M"]
                              [0 0 0 0 0 1 2 3 2]
                              [0 0 0 0 0 0 1 "M" 2]
                              [0 0 0 0 0 0 1 2 "M"]]]
        (calc-board board num-rows num-cols) => calculated-board)
      (let [num-rows 3
            num-cols 3
            board [["M" "M" "M"]
                   ["M" 0 "M"]
                   ["M" "M" "M"]]
            calculated-board [["M" "M" "M"]
                              ["M" 8 "M"]
                              ["M" "M" "M"]]]
        (calc-board board num-rows num-cols) => calculated-board)
      (let [num-rows 3
            num-cols 3
            board [[0 0 0]
                   [0 "M" 0]
                   [0 0 0]]
            calculated-board [[1 1 1]
                              [1 "M" 1]
                              [1 1 1]]]
        (calc-board board num-rows num-cols) => calculated-board)
      (let [num-rows 3
            num-cols 3
            board (vector->board (repeat 9 0) num-rows num-cols)]
        (calc-board board num-rows num-cols) => board)
      (let [num-rows 3
            num-cols 3
            board (vector->board (repeat 9 "M") num-rows num-cols)]
        (calc-board board num-rows num-cols) => board))

(fact "make-mask"
      (let [num-rows 3
            num-cols 5
            mask-3-by-5 [["H" "H" "H" "H" "H"]
                         ["H" "H" "H" "H" "H"]
                         ["H" "H" "H" "H" "H"]]]
        (make-mask num-rows num-cols) => mask-3-by-5))

(fact "flag-cell"
      (fact "flag an unrevealed cell"
            (flag-cell 0 0 [[1]] [["H"]]) => [["F"]]
            (flag-cell 0 0 [["M"]] [["H"]]) => [["F"]])
      (fact "unflag a flagged cell"
            (flag-cell 0 0 [[1]] [["F"]]) => [["H"]]
            (flag-cell 0 0 [["M"]] [["F"]]) => [["H"]])
      (fact "do nothing to a revealed cell"
            (flag-cell 0 0 [[1]] [[1]]) => [[1]]
            (flag-cell 0 0 [["M"]] [["M"]]) => [["M"]]))

; TODO: Write tests for:
; click-cell
; game-over?
; game-won?





