(ns minesweeper-clone.mine-util)

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

(defn flag?
  [mask r c num-rows num-cols]
  (and (in-bounds? r c num-rows num-cols)
       (= "F" (cell-at mask r c))))

(defn unrevealed?
  [mask r c num-rows num-cols]
  (and (in-bounds? r c num-rows num-cols)
       (= "H" (cell-at mask r c))))

(defn count-neighbors
  ([board r c num-rows num-cols]
   (count-neighbors board r c num-rows num-cols mine?))
  ([board r c num-rows num-cols pred?]
   (count
    (filter true?
            [(pred? board r (dec c) num-rows num-cols)
             (pred? board r (inc c) num-rows num-cols)
             (pred? board (dec r) c num-rows num-cols)
             (pred? board (inc r) c num-rows num-cols)
             (pred? board (inc r) (inc c) num-rows num-cols)
             (pred? board (dec r) (inc c) num-rows num-cols)
             (pred? board (inc r) (dec c) num-rows num-cols)
             (pred? board (dec r) (dec c) num-rows num-cols)]))))

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

(defn calc-board
  [num-rows num-cols board]
  (vector->board
   (for [r (range num-rows)
         c (range num-cols)]
     (if (mine? board r c num-rows num-cols)
       (cell-at board r c)
       (count-neighbors board r c num-rows num-cols)))
   num-rows num-cols))

(defn map-mask
  "f is a fn that takes a mask cell and a board cell.
  Maps f over all cells."
  [f board num-rows num-cols mask]
  (let [flat-mask (apply concat mask)
        flat-board (apply concat board)
        flat-mapped-mask (map f flat-mask flat-board)]
    (vector->board flat-mapped-mask num-rows num-cols)))

; assoc-in mask every mine on board
(defn reveal-mines
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if (and (= b "M")
                               (not= m "F")
                               (not= m "B"))
                        b
                        m))
            board num-rows num-cols mask))

; For every cell that is flagged incorrectly,
; assoc-in "X"
(defn reveal-incorrect-flags
  [board num-rows num-cols mask]
  (map-mask (fn [m b] (if (and (= m "F")
                               (not= b "M"))
                        "X"
                        m))
            board num-rows num-cols mask))
