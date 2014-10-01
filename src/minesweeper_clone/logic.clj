(ns minesweeper-clone.logic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]
            [minesweeper-clone.mine-util :refer [random-board
                                                 vector->board
                                                 mine?
                                                 flag?
                                                 unrevealed?
                                                 total-cells
                                                 in-bounds?
                                                 cell-at
                                                 count-neighbors
                                                 calc-board
                                                 map-mask
                                                 reveal-mines
                                                 reveal-incorrect-flags]]))

(defn match-mask
  [vars maskv]
  (if (seq vars)
    (let [m (first maskv)]
      (all
        (if-not (= "H" m)
          (== (first vars) m)
          succeed)
        (match-mask (next vars) (next maskv))))
    succeed))

;; TODO: don't generate lvars for revealed cells (or correctly flagged ones?)

(defn gen-matching-boards
  [[num-rows num-cols num-mines] [r c] mask]
  (let [num-cells (total-cells num-rows num-cols)
        maskv (flatten mask)
        vars (repeatedly num-cells lvar)
        vars-board (vector->board vars num-rows num-cols)
        ]
    (run* [q] ;;try run* or run 1
          (== q vars)
          (everyg #(membero % [0 1 2 3 4 5 6 7 8 "M"]) vars)
          (match-mask vars maskv)
          (!= (get-in vars-board [r c]) "M")
          (project [vars]
                   (let [vars-board (vector->board vars num-rows num-cols)]
                     (== num-mines (count (filter #{"M"} vars)))
                     (== vars-board
                         (calc-board num-rows num-cols vars-board))
                     ))
          )))

(defn run-logic
  []
  (gen-matching-boards [2 2 2]
                       [1 1]
                       [["H" "H"]
                        [2 "H"]])
  ; (gen-matching-boards [3 3 3]
  ;                      ;[2 2]
  ;                      [0 0]
  ;                      [[1 "H" "H"]
  ;                       [1 3 3]
  ;                       ["H" "H" "H"]])
  )











