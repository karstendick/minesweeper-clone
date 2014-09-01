(ns minesweeper-clone.core
  (:require [minesweeper-clone.mine-util :refer [random-board
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

;; [num-rows num-cols num-mines]
(def board-defaults {:beginner     [9 9 10]
                     :intermediate [16 16 40]
                     :expert       [16 30 99]})

(defn make-board
  [num-rows num-cols num-mines]
  (->> (random-board num-rows num-cols num-mines)
       (calc-board num-rows num-cols)))

(defn make-mask
  [num-rows num-cols]
  (let [num-cells (total-cells num-rows num-cols)
        v (vec (repeat num-cells "H"))]
    (vector->board v num-rows num-cols)))

(declare click-cell)

(defn- click-neighbors
  [r c board num-rows num-cols mask]
  (->> mask
       (click-cell r (dec c) board num-rows num-cols)
       (click-cell r (inc c) board num-rows num-cols)
       (click-cell (dec r) c board num-rows num-cols)
       (click-cell (inc r) c board num-rows num-cols)
       (click-cell (inc r) (inc c) board num-rows num-cols)
       (click-cell (dec r) (inc c) board num-rows num-cols)
       (click-cell (inc r) (dec c) board num-rows num-cols)
       (click-cell (dec r) (dec c) board num-rows num-cols)))

(defn click-cell
  [r c board num-rows num-cols mask]
  (let [board-cell (cell-at board r c)
        mask-cell (cell-at mask r c)]
    (if (or (= "F" mask-cell) ; Flagged cell
            (not (in-bounds? r c num-rows num-cols)) ; Not a valid cell
            (= board-cell mask-cell)) ; Cell already revealed
      mask
      (if (= 0 board-cell)
        ; Click this cell and all its neighbors
        (->> (assoc-in mask [r c] board-cell)
             (click-neighbors r c board num-rows num-cols))
        (if (mine? board r c num-rows num-cols) ; Cell is  a mine
          (assoc-in mask [r c] "B") ; Boom!
          ; Otherwise, just reveal this cell
          (assoc-in mask [r c] board-cell))))))

(defn flag-cell
  [r c board mask]
  (condp = (cell-at mask r c)
    "H" (assoc-in mask [r c] "F") ; flag an unrevealed cell
    "F" (assoc-in mask [r c] "H") ; unflag a flagged cell
    mask)) ; do nothing otherwise

; Should chord-click-cell just be rolled into click-cell?
; (iOS app does this; Windows does not)
; Keep this separate for now.
; We can always do both, then, based on a user setting.

; If the current cell is a number
; and that number is equal to the number of flagged neighbors
; and all flagged neighbors are correct,
; reveal all neighbors of this cell (using click-cell fn)

(defn chord-click-cell
  [r c board num-rows num-cols mask]
  (let [mask-cell (cell-at mask r c)
        neighboring-flags (count-neighbors mask r c num-rows num-cols flag?)]
    (if (or (unrevealed? mask r c num-rows num-cols)
            (flag? board r c num-rows num-cols)
            (not= mask-cell neighboring-flags))
      mask
      (click-neighbors r c board num-rows num-cols mask))))

(defn game-over?
  [mask]
  (some #{"B"} (apply concat mask)))

(defn game-won?
  [board num-rows num-cols mask]
  (and (not (game-over? mask))
       (let [cell-won? (fn [m b]
                         (or (= b m)     ; cell is revealed
                             (= "M" b))) ; cell is a mine (Don't require mines to be flagged.)
             mapped-mask (map-mask cell-won? board num-rows num-cols mask)
             flat-mapped-mask (apply concat mapped-mask)]
         (every? true? flat-mapped-mask))))

(defn calc-game-over
  [board num-rows num-cols mask]
  (->> mask
       (reveal-mines board num-rows num-cols)
       (reveal-incorrect-flags board num-rows num-cols)))

; For every mine that is not yet flagged, flag it
(defn calc-game-won
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if  (= b "M")
                        "F"
                        m))
            board num-rows num-cols mask))

(defn print-board
  [board]
  (doseq [row board]
    (println row)))

(defn get-input
  [prompt]
  (println prompt "--> ")
  (read-line))

(defn sample-board
  []
  (let [[num-rows num-cols num-mines] (:beginner board-defaults)]
    (print-board (random-board num-rows num-rows num-mines))))

(defn random-beginner-board
  []
  (let [[num-rows num-cols num-mines] (:beginner board-defaults)]
    (random-board num-rows num-cols num-mines)))

; TODO: Keep track of # of mines left unflagged:
; this starts at num-mines,
; goes down every time you flag and unflagged cell,
; (goes up when you unflag a flagged cell)
; and will be 0 when game-won? is true

; TODO: Keep track of time.
; TODO: Save best times for each difficulty level.


; TODO: If the first click of the game is on a mine,
; generate a new board until it's not. You can't lose on the first click.
(defn play-game
  []
  (let [[num-rows num-cols num-mines] (:beginner board-defaults)]
    (loop [board (make-board num-rows num-cols num-mines)
           mask (make-mask num-rows num-cols)]
      (print-board board)
      (println)
      (print-board mask)
      (if (game-won? board num-rows num-cols mask)
        (do
          (println "You won!")
          (print-board (calc-game-won board num-rows num-cols mask)))
        (if (game-over? mask)
          (do
            (println "Game over!")
            (print-board (calc-game-over board num-rows num-cols mask)))
          ;; Using read-string like this is super dangerous!
          (let [[r c op] (map read-string (clojure.string/split (get-input "row col") #"\s+"))]
            (if (= r 'q)
              (println "Good bye!")
              (if (= op 'f)
                (recur board (flag-cell r c board mask))
                (if (= op 'c)
                  (recur board (chord-click-cell r c board num-rows num-cols mask))
                  (recur board (click-cell r c board num-rows num-cols mask)))))))))))



