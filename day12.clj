(def input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(require '[clojure.string])
(require '[clojure.set])

(def move #(mapv + %1 %2))

;; Part One

(defn possible-positions [from board]
  (let [dirs [[1 0] [0 1] [-1 0] [0 -1]]
        positions (mapv #(move from %) dirs)
        no-negatives (remove #(some neg? %) positions)
        [height width] ((juxt #(count %) #(count (first %))) (clojure.string/split-lines input))
        on-board (filter (fn [[r c]] (and (< r height) (< c width))) no-negatives)
        reachable? #(> 2 (- (int (board %)) (int (board from))))]
    (filter reachable? on-board)))

(def board
  (->> input
    (clojure.string/split-lines)
    (#(for [i (range (count %))
            j (range (count (first %)))
            :let [c (get-in % [i j])]]
        (cond
          (= c \S) {:current [i j]
                    [i j] \a}
          (= c \E) {:target [i j]
                    [i j] \z}
          :else    {[i j] c})))
    (apply merge)))

(loop [counter 0
       next-positions #{(board :current)}
       visited #{}]
  ;; (println counter)
  ;; (println next-positions)
  ;; (println)
  (if (some #{(board :target)} next-positions)
    counter
    (recur (inc counter)
           (clojure.set/difference (set (mapcat #(possible-positions % board) next-positions))
                                   visited)
           (clojure.set/union visited next-positions))))

;; Part Two

(defn possible-positions-2 [from board]
  (let [dirs [[1 0] [0 1] [-1 0] [0 -1]]
        positions (mapv #(move from %) dirs)
        no-negatives (remove #(some neg? %) positions)
        [height width] ((juxt #(count %) #(count (first %))) (clojure.string/split-lines input))
        on-board (filter (fn [[r c]] (and (< r height) (< c width))) no-negatives)
        reachable? #(> 2 (- (int (board from)) (int (board %))))] ;; Only changes the order of subtraction, it's "from - %" instead of "% - from" ;; Only changes the order of subtraction, it's "from - %" instead of "% - from".
    (filter reachable? on-board)))

(def board-2
  (->> input
    (clojure.string/split-lines)
    (#(for [i (range (count %))
            j (range (count (first %)))
            :let [c (get-in % [i j])]]
        (cond
          (= c \E) {:current [i j]
                    [i j] \z}
          :else    {[i j] c})))
    (apply merge)))

(loop [counter 0
       next-positions #{(board-2 :current)}
       visited #{}]
  ;; (println counter)
  ;; (println next-positions)
  ;; (println)
  (if (some #(= \a (board-2 %)) next-positions)
    counter
    (recur (inc counter)
           (clojure.set/difference (set (mapcat #(possible-positions-2 % board-2) next-positions))
                                   visited)
           (clojure.set/union visited next-positions))))
