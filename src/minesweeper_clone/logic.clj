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

(defn run-logic
  []
  (let [[num-rows num-cols num-mines] [2 2 2]
        num-cells (total-cells num-rows num-cols)
        boardv [2 "M" 2 "M"]
        board (vector->board boardv num-rows num-cols)
        maskv ["H" "H" 2 "H"]
        mask (vector->board maskv num-rows num-cols)
        [r c] [1 1]
        vars (repeatedly num-cells lvar)
        vars-board (vector->board vars num-rows num-cols)
        ;_ (println vars)
        ]
    (run* [q] ;;try run*
         (== q vars)
         (everyg #(membero % [0 1 2 3 "M"]) vars)
         (membero (get-in vars-board [r c]) [0 1 2 3])
         (match-mask vars maskv)
         (project [vars]
                  (let [vars-board (vector->board vars num-rows num-cols)]
                    (== num-mines (count (filter #{"M"} vars)))
                    (== vars-board
                        (calc-board num-rows num-cols vars-board))
                    ))
         ;(everyg #(membero % [0 "M"]) vars)
         ;(== num-mines (count (filter #(== % 9) vars)))
         ;;(membero 9)

         )))