(def input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def input2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def dir
  {"R" [1 0]
   "U" [0 1]
   "L" [-1 0]
   "D" [0 -1]})

;;;; Head - Tail
;; (map - [0 3] [2 2])

;; n222n.
;; 21112.
;; 21T12.
;; 21112.
;; n222n.

(def tail-dir-by-diff
  {[-2 2] [-1 1] ;; included in part two
   [-1 2] [-1 1]
   [0 2] [0 1]
   [1 2] [1 1]
   [2 2] [1 1] ;; included in part two
   [-2 1] [-1 1]
   [-1 1] [0 0]
   [0 1] [0 0]
   [1 1] [0 0]
   [2 1] [1 1]
   [-2 0] [-1 0]
   [-1 0] [0 0]
   [0 0] [0 0]
   [1 0] [0 0]
   [2 0] [1 0]
   [-2 -1] [-1 -1]
   [-1 -1] [0 0]
   [0 -1] [0 0]
   [1 -1] [0 0]
   [2 -1] [1 -1]
   [-2 -2] [-1 -1] ;; included in part two
   [-1 -2] [-1 -1]
   [0 -2] [0 -1]
   [1 -2] [1 -1]
   [2 -2] [1 -1]}) ;; included in part two

;; Part One

(require '[clojure.string :as string])
(->> input
  (string/split-lines)
  (map #(re-find #"(.) (\d+)" %))
  (map #(into [(second %) (Integer/parseInt (nth % 2))]))
  (map #(repeat (second %) (dir (first  %))))
  (flatten)
  (partition 2)
  (reduce
    (fn [{:keys [head-positions tail-positions]} head-move-dir]
      (let [move #(map + %1 %2)
            new-head-position (move (last head-positions) head-move-dir)
            head-diff-tail (map - new-head-position (last tail-positions))
            tail-move-dir (tail-dir-by-diff head-diff-tail)
            new-tail-position (move (last tail-positions) tail-move-dir)]
        {:head-positions (conj head-positions new-head-position)
         :tail-positions (conj tail-positions new-tail-position)}))
    {:head-positions [[0 0]]
     :tail-positions [[0 0]]})
  (:tail-positions)
  (distinct)
  (count))


;; Part Two

(def move #(map + %1 %2))

(defn move-knots [head-knot-positions tail-positions]
  (let [head-position (last (last head-knot-positions))
        tail-position (last tail-positions)
        head-diff-tail (map - head-position tail-position)
        tail-move-dir (tail-dir-by-diff head-diff-tail)
        new-tail-position (move tail-position tail-move-dir)
        new-tail-positions (conj tail-positions new-tail-position)]
    (conj head-knot-positions new-tail-positions)))

(->> input
  (string/split-lines)
  (map #(re-find #"(.) (\d+)" %))
  (map #(into [(second %) (Integer/parseInt (nth % 2))]))
  (map #(repeat (second %) (dir (first  %))))
  (flatten)
  (partition 2)
  (reduce
    (fn [knots-positions head-move-dir]
      (let [new-head-position (move (last (first knots-positions)) head-move-dir)
            new-head-positions (conj (first knots-positions) new-head-position)]
        (reduce move-knots [new-head-positions] (rest knots-positions))))
    (vec (repeat 10 [[0 0]])))
  ;; (map-indexed #(into {%1 (last %2)})))
  ;; (map count)
  (#(nth % 9))
  (distinct)
  (count))
