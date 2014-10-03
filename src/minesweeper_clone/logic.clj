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
  [boardv maskv]
  (if (seq boardv)
    (let [m (first maskv)]
      (all
        (if-not (= "H" m)
          (== (first boardv) m)
          succeed)
        (match-mask (next boardv) (next maskv))))
    succeed))



;; TODO: should board and mask have the old values,
;; or should we already account for changing things at clicked cell?

(defn get-new-mine-position
  [[num-rows num-cols num-mines] [r c] board mask]
  (let [num-cells (total-cells num-rows num-cols)
        maskv (flatten mask)
        ; TODO: don't shadow the board name like this
        ; remove the old mine
        board (assoc-in board [r c] 0)
        ]
    (run* [row-var col-var] ;;try run* or run 1
          (membero row-var (range num-rows))
          (membero col-var (range num-cols))
          ;(match-mask vars maskv)
          (!= [row-var col-var]
              [r c]) ; Don't place the mine back here


          (project [row-var col-var]
                   ; Place the mine in an unrevealed cell
                   (== (get-in mask [row-var col-var]) "H")
                   (let [var-board (->> (assoc-in board [row-var col-var] "M")
                                        (calc-board num-rows num-cols)
                                        flatten)
                         _ (println)
                         _ (println row-var col-var)
                         _ (println "var-board: " var-board)
                         _ (println "maskv:     " maskv)
                         ]
                     (match-mask var-board maskv)
                     )

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
                         [[0 "M" "M"]
                          [0 0 0]
                          [0 0 "M"]]
                         [[1 "H" "H"]
                          ["H" 3 3]
                          ["H" "H" "H"]])
  )











