(ns minesweeper-clone.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; [num-rows num-cols num-mines]
(def board-defaults {:beginner     [9 9 10]
                     :intermediate [16 16 40]
                     :expert       [16 30 99]})
(def num-rows 9)
(def num-cols 9)
(def num-mines 10)

(defn total-cells
  [num-rows num-cols]
  (* num-rows num-cols))

(defn in-bounds?
  [r c]
  (and (<= 0 r (dec num-rows))
       (<= 0 c (dec num-cols))))


(defn cell-at
  "Takes a vector of vectors and returns the value at
  the given row and column"
  [vv r c]
  (get-in vv [r c]))

(defn mine?
  [board r c num-rows num-cols]
  (and (in-bounds? r c)
       (= "M" (cell-at board r c))))

(defn count-neighbors
  [board r c]
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

; (def b [[0 1 2]
;         [3 4 5]
;         [6 7 8]])

; Assert that v has num-rows*num-cols entries
(defn vector-to-board
  [v num-rows num-cols]
  (into [] (map vec (partition num-cols v))))

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
        random-board (vector-to-board random-vector num-rows num-cols)]
    random-board))

(defn calc-board
  [board num-rows num-cols]
  (vector-to-board
   (for [r (range num-rows)
         c (range num-cols)]

     (if (mine? board r c num-rows num-cols)
       (cell-at board r c)
       (count-neighbors board r c)))
   num-rows num-cols))

(defn print-board
  [board]
  (doseq [row board]
    (println row)))

(defn sample-board
  []
  (print-board (random-board num-rows num-rows num-mines)))

(defn make-mask
  [num-rows num-cols]
  (let [num-cells (total-cells num-rows num-cols)
        v (into [] (repeat num-cells "H"))]
    (vector-to-board v num-rows num-cols)))

; TODO: Change nested ifs to conds
(defn click-cell
  [r c board mask]
  (if (or (= "F" (cell-at mask r c)) ; Flagged cell
          (not (in-bounds? r c))     ; Not a valid cell
          (= (cell-at board r c)     ; Cell already revealed
             (cell-at mask r c)))
    mask
    (if (= 0 (cell-at board r c))
      ; Click this cell and
      ; recursively click all its neighbors
      (->> (assoc-in mask [r c] (cell-at board r c))
           (click-cell r (dec c) board)
           (click-cell r (inc c) board)
           (click-cell (dec r) c board)
           (click-cell (inc r) c board)
           (click-cell (inc r) (inc c) board)
           (click-cell (dec r) (inc c) board)
           (click-cell (inc r) (dec c) board)
           (click-cell (dec r) (dec c) board))
      ; Reveal this cell
      (assoc-in mask [r c] (cell-at board r c)))))

; TODO: Change nested ifs to conds
(defn flag-cell
  [r c board mask]
  (if (= "F" (cell-at mask r c))
    (assoc-in mask [r c] "H")
    (if (= "H" (cell-at mask r c))
    (assoc-in mask [r c] "F")
    mask)))

(defn game-over?
  [board mask r c]
  (and (= "H" (cell-at mask r c))            ; cell is unrevealed
       (mine? board r c num-rows num-cols))) ; cell has a mine

; TODO: Write a game-won? function and wire it in

(defn get-input
  [prompt]
  (println prompt "--> ")
  (read-line))

(defn play-game
  []
  (loop [board (calc-board (random-board num-rows num-cols num-mines) num-rows num-cols)
         mask (make-mask num-rows num-cols)]
    (print-board board)
    (println)
    (print-board mask)
    ;; Using read-string like this is super dangerous!
    (let [[r c op] (map read-string (clojure.string/split (get-input "row col") #"\s+"))]
      (if (or (= r 'q)
              (= c 'q)
              (= op 'q))
        (println "Good bye!")
        (if (= op 'f)
          (recur board (flag-cell r c board mask))
          (if (game-over? board mask r c)
            (println "Game over!")
            (recur board (click-cell r c board mask))))))))



