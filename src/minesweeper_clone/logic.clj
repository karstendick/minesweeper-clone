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



;; TODO: should board and mask have the old values,
;; or should we already account for changing things at clicked cell?

(defn get-new-mine-position
  [[num-rows num-cols num-mines] [r c] board mask]
  (let [num-cells (total-cells num-rows num-cols)
        maskv (flatten mask)
        vars (repeatedly 2 lvar)
        ]
    (run* [q] ;;try run* or run 1
          (== q vars)
          (membero (first vars) (range num-rows))
          (membero (second vars) (range num-cols))
          ;(match-mask vars maskv)
          (!= vars [r c]) ; Don't place the mine back here

          (project [vars]
                   ; Place the mine in an unrevealed cell
                   (== (get-in mask vars) "H")

                   ; (let [;_ (println "project vars: " vars)
                   ;       vars-board (vector->board vars num-rows num-cols)]
                   ;   (== num-mines (count (filter #{"M"} vars)))
                   ;   (== vars-board
                   ;       (calc-board num-rows num-cols vars-board))
                   ;   )
                   ))))

(defn run-logic
  []
  (get-new-mine-position [2 2 2]
                         [1 1]
                         [[2 "M"]
                          [2 "M"]]
                         [["H" "H"]
                          [2 "H"]]
                         )
  (get-new-mine-position [3 3 3]
                       [2 2]
                       [] ; TODO: Fill this in?
                       [[1 "H" "H"]
                        ["H" 3 3]
                        ["H" "H" "H"]])
  )











