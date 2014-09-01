(ns minesweeper-clone.core-test
  (:require [minesweeper-clone.core :refer :all]
            [midje.sweet :refer :all]
            [midje.util :refer [testable-privates]]))

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

(fact "click-cell"
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            mask [["H" "H" "H"]
                  ["H" "H" 2]
                  ["H" "H" "F"]]
            nr 3
            nc 3]
        (fact "does nothing"
              (fact "to flagged cell"
                    (click-cell 2 2 board nr nc mask) => mask)
              (fact "to invalid cell"
                    (click-cell 2 3 board nr nc mask) => mask)
              (fact "to revealed cell"
                    (click-cell 1 2 board nr nc mask) => mask))
        (fact "goes Boom! to unrevealed mine cell"
              (click-cell 0 2 board nr nc mask) => (assoc-in mask [0 2] "B"))
        (fact "reveals a non-zero numbered cell"
              (click-cell 0 1 board nr nc mask) => (assoc-in mask [0 1] 1)
              (click-cell 1 2 board nr nc mask) => (assoc-in mask [1 2] 2))
        (fact "reveals all neighbors of a zero cell"
              (click-cell 0 0 board nr nc mask)
              => [[0 1 "H"]
                  [0 2 2]
                  [0 1 "F"]])))

(fact "chord-click-cell"
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            mask [["H" "H" "H"]
                  ["H" "H" 2]
                  ["H" 1 "F"]]
            nr 3
            nc 3]
        (fact "does nothing"
              (fact "to unrevealed cell"
                    (chord-click-cell 0 0 board nr nc mask) => mask
                    (chord-click-cell 0 2 board nr nc mask) => mask)
              (fact "to flagged cell"
                    (chord-click-cell 2 2 board nr nc mask) => mask)
              (fact "to cell without enough neighboring flags"
                    (chord-click-cell 1 2 board nr nc mask) => mask)
              (let [over-flagged-board [[0 1]
                                        [1 "M"]]
                    over-flagged-mask [["H" "F"]
                                       [1 "F"]]]
                (fact "to cell with too many neighboring flags"
                      (chord-click-cell 1 0 over-flagged-board nr nc over-flagged-mask)
                      => over-flagged-mask)))
        (fact "goes boom to wrong flag"
              (chord-click-cell 1 2 board nr nc [["H" "F" "H"]
                                                 ["H" "H" 2]
                                                 ["H" 1 "F"]])
              => [["H" "F" "B"]
                  ["H" 2 2]
                  ["H" 1 "F"]])
        (fact "reveals all neighbors"
              (chord-click-cell 2 1 board nr nc mask)
              => [[0 1 "H"]
                  [0 2 2]
                  [0 1 "F"]])))

(fact "game-over?"
      (game-over? [["B"]]) => truthy
      (game-over? [["H"]]) => falsey
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            mask [[0 1 "H"]
                  [0 2 2]
                  [0 1 "F"]]
            nr 3
            nc 3]
        (game-over? mask) => falsey
        (->> mask
             (click-cell 0 2 board nr nc)
             game-over?)
        => truthy))

(fact "game-won?"
      (game-won? [[0]] 1 1 [[0]]) => true
      (game-won? [["M"]] 1 1 [["H"]]) => true
      (game-won? [["M"]] 1 1 [["F"]]) => true
      (game-won? [[1 "M"]] 1 1 [[1 "H"]]) => true
      (game-won? [[1 "M"]] 1 1 [[1 "F"]]) => true
      (game-won? [[1 "M"]] 1 1 [["H" "F"]]) => false
      (game-won? [[1 "M"]] 1 1 [[1 "B"]]) => false
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            mask [[0 1 "H"]
                  [0 2 2]
                  [0 1 "F"]]
            nr 3
            nc 3]
        (game-won? board nr nc mask) => true
        (->> mask
             (click-cell 0 2 board nr nc)
             (game-won? board nr nc))
        => false))

(fact "calc-game-won"
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            nr 3
            nc 3]
        (calc-game-won board nr nc [[0 1 "H"]
                                    [0 2 2]
                                    [0 1 "F"]])
        => [[0 1 "F"]
            [0 2 2]
            [0 1 "F"]]
        (calc-game-won board nr nc [[0 1 "F"]
                                    [0 2 2]
                                    [0 1 "F"]])
        => [[0 1 "F"]
            [0 2 2]
            [0 1 "F"]]))

(fact "calc-game-over"
      (let [board [[0 1 "M"]
                   [0 2 2]
                   [0 1 "M"]]
            nr 3
            nc 3]
        (calc-game-over board nr nc [[0 1 "H"]
                                     [0 3 2]
                                     ["B" 2 "F"]])
        => [[0 1 "M"]
            [0 3 2]
            ["B" 2 "F"]]
        (->> [[0 1 "F"]
              [0 2 "F"]
              [0 1 "H"]]
             (chord-click-cell 1 1 board nr nc)
             (calc-game-over board nr nc))
        => [[0 1 "F"]
            [0 2 "X"]
            [0 1 "B"]]
        (calc-game-over board nr nc [[0 1 "F"]
                                     [0 2 "F"]
                                     [0 1 "H"]])
        => [[0 1 "F"]
            [0 2 "X"]
            [0 1 "M"]]))








