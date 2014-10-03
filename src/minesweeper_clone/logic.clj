(ns minesweeper-clone.logic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]
            [minesweeper-clone.mine-util :refer [calc-board]]))

; TODO: What about flagged cells?
(defn- match-mask
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
  [[num-rows num-cols] [r c] board mask]
  (let [maskv (flatten mask)
        ; remove the old mine
        ; TODO: don't shadow the board name like this
        board (assoc-in board [r c] 0)
        ]
    ; TODO: Change this to run 1 when it's fully tested
    ; Or maybe use run* and then randomly select one?
    (run* [row-var col-var] ;;try run* or run 1
          (membero row-var (range num-rows))
          (membero col-var (range num-cols))
          (!= [row-var col-var]
              [r c]) ; Don't place the mine back here
          (project [row-var col-var]
                   ; Place the mine in an unrevealed cell
                   (== (get-in mask [row-var col-var]) "H")
                   ; Make sure the revealed cells match the new board
                   (let [var-boardv (->> (assoc-in board [row-var col-var] "M")
                                        (calc-board num-rows num-cols)
                                        flatten)
                         ; _ (println)
                         ; _ (println row-var col-var)
                         ; _ (println "var-boardv: " var-boardv)
                         ; _ (println "maskv:      " maskv)
                         ]
                     (match-mask var-boardv maskv))))))

