(ns minesweeper-clone.core)

;; [num-rows num-cols num-mines]
(def board-defaults {:beginner     [9 9 10]
                     :intermediate [16 16 40]
                     :expert       [16 30 99]})
;(def num-rows 9)
;(def num-cols 9)
;(def num-mines 10)
;(def num-mines 1)
;(def num-mines 81)
;(def num-mines 0)

(defn total-cells
  [num-rows num-cols]
  (* num-rows num-cols))

(defn in-bounds?
  [r c num-rows num-cols]
  (and (<= 0 r (dec num-rows))
       (<= 0 c (dec num-cols))))


(defn cell-at
  "Takes a vector of vectors and returns the value at
  the given row and column"
  [vv r c]
  (get-in vv [r c]))

(defn mine?
  [board r c num-rows num-cols]
  (and (in-bounds? r c num-rows num-cols)
       (= "M" (cell-at board r c))))

(defn count-neighbors
  [board r c num-rows num-cols]
  (count
   (filter true?
           [(mine? board r (dec c) num-rows num-cols)
            (mine? board r (inc c) num-rows num-cols)
            (mine? board (dec r) c num-rows num-cols)
            (mine? board (inc r) c num-rows num-cols)
            (mine? board (inc r) (inc c) num-rows num-cols)
            (mine? board (dec r) (inc c) num-rows num-cols)
            (mine? board (inc r) (dec c) num-rows num-cols)
            (mine? board (dec r) (dec c) num-rows num-cols)])))

; Assert that v has num-rows*num-cols entries
(defn vector->board
  [v num-rows num-cols]
  (vec (map vec (partition num-cols v))))

; Not a pure function, of course,
; as this generates a random different board each time.
;
; Assert that num-mines < num-rows*num-cols
(defn random-board
  [num-rows num-cols num-mines]
  (let [num-cells (total-cells num-rows num-cols)
        num-empty-cells (- num-cells num-mines)
        unrandom-vector (into (repeat num-mines "M")
                              (repeat num-empty-cells "_"))
        random-vector (shuffle unrandom-vector)
        random-board (vector->board random-vector num-rows num-cols)]
    random-board))

(defn random-beginner-board
  []
  (let [[num-rows num-cols num-mines] (:beginner board-defaults)]
    (random-board num-rows num-cols num-mines)))

(defn calc-board
  [board num-rows num-cols]
  (vector->board
   (for [r (range num-rows)
         c (range num-cols)]
     (if (mine? board r c num-rows num-cols)
       (cell-at board r c)
       (count-neighbors board r c num-rows num-cols)))
   num-rows num-cols))

(defn print-board
  [board]
  (doseq [row board]
    (println row)))

(defn sample-board
  []
  (let [[num-rows num-cols num-mines] (:beginner board-defaults)]
    (print-board (random-board num-rows num-rows num-mines))))

(defn make-mask
  [num-rows num-cols]
  (let [num-cells (total-cells num-rows num-cols)
        v (vec (repeat num-cells "H"))]
    (vector->board v num-rows num-cols)))

(defn click-cell
  [r c board num-rows num-cols mask]
  (if (or (= "F" (cell-at mask r c)) ; Flagged cell
          (not (in-bounds? r c num-rows num-cols)) ; Not a valid cell
          (= (cell-at board r c) ; Cell already revealed
             (cell-at mask r c)))
    mask
    (if (= 0 (cell-at board r c))
      ; Click this cell and
      ; recursively click all its neighbors
      (->> (assoc-in mask [r c] (cell-at board r c))
           (click-cell r (dec c) board num-rows num-cols)
           (click-cell r (inc c) board num-rows num-cols)
           (click-cell (dec r) c board num-rows num-cols)
           (click-cell (inc r) c board num-rows num-cols)
           (click-cell (inc r) (inc c) board num-rows num-cols)
           (click-cell (dec r) (inc c) board num-rows num-cols)
           (click-cell (inc r) (dec c) board num-rows num-cols)
           (click-cell (dec r) (dec c) board num-rows num-cols))
      (if (= "M" (cell-at board r c)) ; Cell is  a mine
        (assoc-in mask [r c] "B") ; Boom!
        ; Otherwise, just reveal this cell
        (assoc-in mask [r c] (cell-at board r c))))))

(defn flag-cell
  [r c board mask]
  (condp = (cell-at mask r c)
    "H" (assoc-in mask [r c] "F") ; flag an unrevealed cell
    "F" (assoc-in mask [r c] "H") ; unflag a flagged cell
    mask)) ; do nothing otherwise

; Should this just be rolled into click-cell? (iOS app does this; Windows does not)
; TODO: Write middle-click-cell fn
; If the current cell is a number
; and that number is equal to the number of flagged neighbors
; and all flagged neighbors are correct,
; reveal all neighbors of this cell (using click-cell fn)

(defn game-over?
  [mask]
  (some #{"B"} (apply concat mask)))

(defn map-mask
  "f is a fn that takes a mask cell and a board cell.
  Maps f over all cells."
  [f board num-rows num-cols mask]
  (let [flat-mask (apply concat mask)
        flat-board (apply concat board)
        flat-mapped-mask (map f flat-mask flat-board)]
    (vector->board flat-mapped-mask num-rows num-cols)))

(defn game-won?
  [board num-rows num-cols mask]
  (and (not (game-over? mask))
       (let [cell-won? (fn [m b]
                         (or (= b m)     ; cell is revealed
                             (= "M" b))) ; cell is a mine (Don't require mines to be flagged.)
             mapped-mask (map-mask cell-won? board num-rows num-cols mask)
             flat-mapped-mask (apply concat mapped-mask)]
         (every? true? flat-mapped-mask))))

(defn get-input
  [prompt]
  (println prompt "--> ")
  (read-line))

; TODO: test calc-game-over
; assoc-in mask every mine on board
; For every cell that is flagged incorrectly,
; assoc-in "X"
(defn reveal-mines
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if (= b "M")
                        b
                        m))
            board num-rows num-cols mask))

(defn reveal-incorrect-flags
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if (and (= m "F")
                               (not= b "M"))
                        "X"
                        m))
            board num-rows num-cols mask))

(defn calc-game-over
  [mask board num-rows num-cols]
  (->> mask
       (reveal-mines board num-rows num-cols)
       (reveal-incorrect-flags board num-rows num-cols)))

; TODO: test calc-game-won
; For every cell that is not yet flagged,
; flag it
(defn calc-game-won
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if  (= b "M")
                        "F"
                        m))
            board num-rows num-cols mask))

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
    (loop [board (calc-board (random-board num-rows num-cols num-mines) num-rows num-cols)
           mask (make-mask num-rows num-cols)]
      (print-board board)
      (println)
      (print-board mask)
      (if (game-won? board num-rows num-cols mask)
        (println "You won!")
        (if (game-over? mask)
          (println "Game over!")
          ;; Using read-string like this is super dangerous!
          (let [[r c op] (map read-string (clojure.string/split (get-input "row col") #"\s+"))]
            (if (or (= r 'q)
                    (= c 'q)
                    (= op 'q))
              (println "Good bye!")
              (if (= op 'f)
                (recur board (flag-cell r c board mask))
                (recur board (click-cell r c board num-rows num-cols mask))))))))))



