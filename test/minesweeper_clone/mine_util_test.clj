(ns minesweeper-clone.mine-util-test
  (:require [minesweeper-clone.mine-util :refer :all]
            [midje.sweet :refer :all]))

(fact "in-bounds?"
      (let [[num-rows num-cols] [9 9]]
        (in-bounds? 0 0 num-rows num-cols) => true
        (in-bounds? 1 1 num-rows num-cols) => true
        (in-bounds? -1 0 num-rows num-cols) => false
        (in-bounds? 0 -1 num-rows num-cols) => false
        (in-bounds? (inc num-rows) 0 num-rows num-cols) => false
        (in-bounds? 0 (inc num-cols) num-rows num-cols) => false))

(fact "count-neighbors"
      (let [[num-rows num-cols] [3 5]
            board [[0 "M" "M" "M" 0]
                   [0 0 0 0 0]
                   [0 0 0 0 0]]]
        (count-neighbors board 0 0 num-rows num-cols) => 1
        (count-neighbors board 1 1 num-rows num-cols) => 2
        (count-neighbors board 1 2 num-rows num-cols) => 3))

(fact "vector->board"
      (let [[num-rows num-cols] [3 5]
            v (range 15)
            board [(range 0 5)
                   (range 5 10)
                   (range 10 15)]]
        (vector->board v num-rows num-cols) => board))

(fact "calc-board"
      (let [[num-rows num-cols] [9 9]
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
        (calc-board num-rows num-cols board) => calculated-board
        (fact "is idempotent"
              (calc-board num-rows num-cols calculated-board) => calculated-board))
      (let [[num-rows num-cols] [3 3]
            board [["M" "M" "M"]
                   ["M" 0 "M"]
                   ["M" "M" "M"]]
            calculated-board [["M" "M" "M"]
                              ["M" 8 "M"]
                              ["M" "M" "M"]]]
        (calc-board num-rows num-cols board) => calculated-board)
      (let [[num-rows num-cols] [3 3]
            board [[0 0 0]
                   [0 "M" 0]
                   [0 0 0]]
            calculated-board [[1 1 1]
                              [1 "M" 1]
                              [1 1 1]]]
        (calc-board num-rows num-cols board) => calculated-board)
      (let [num-rows 3
            num-cols 3
            board (vector->board (repeat 9 0) num-rows num-cols)]
        (calc-board num-rows num-cols board) => board)
      (let [num-rows 3
            num-cols 3
            board (vector->board (repeat 9 "M") num-rows num-cols)]
        (calc-board num-rows num-cols board) => board))
