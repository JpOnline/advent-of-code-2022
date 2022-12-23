(def input "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(do
  (defn print-board [{:keys [board]}]
    (let [min-x (apply min (map first board))
          min-y (apply min (map second board))
          max-x (apply max (map first board))
          max-y (apply max (map second board))]
      (remove nil?
        (for [y (range min-y (inc max-y))
              x (range min-x (inc max-x))]
          (do
            (print (if (board [x y]) \# \.))
            (when (= x max-x) (print \newline)))))))

  (def E [1 0])
  (def S [0 1])
  (def W [-1 0])
  (def N [0 -1])
  (def NE [1 -1])
  (def NW [-1 -1])
  (def SE [1 1])
  (def SW [-1 1])

  (defn move [{:keys [board dirs] :as state}]
    (let [no-one-in-dir? (fn [elf testing-dirs] (every? nil? (map #(board (map + elf %)) testing-dirs)))
          no-one-around? #(no-one-in-dir? % [E S W N NE NW SE SW])
          propose #(cond
                     (no-one-around? %) [% %]
                     (no-one-in-dir? % (first dirs)) [% (mapv + % (ffirst dirs))]
                     (no-one-in-dir? % (get dirs 1)) [% (mapv + % (get-in dirs [1 0]))]
                     (no-one-in-dir? % (get dirs 2)) [% (mapv + % (get-in dirs [2 0]))]
                     (no-one-in-dir? % (get dirs 3)) [% (mapv + % (get-in dirs [3 0]))]
                     :else [% %])
          propositions (mapv propose board)
          dups (set (map first (filter (fn [[_ f]] (> f 1)) (frequencies (map second propositions)))))]
      (-> state
        (update :counter inc) ;; Added for Part Two
        (assoc :board (set (map (fn [[o p]] (if (dups p) o p)) propositions)))
        (update :dirs (fn [[d & r-d]] (conj (vec r-d) d))))))

  (defn count-empty [{:keys [board]}]
    (let [min-x (apply min (map first board))
          min-y (apply min (map second board))
          max-x (apply max (map first board))
          max-y (apply max (map second board))]
      (- (* (- (inc max-x) min-x) (- (inc max-y) min-y)) (count board))))

  (->> input
    (clojure.string/split-lines)
    (#(map-indexed
        (fn [row line]
          (map-indexed
            (fn [column c]
              [(- column (quot (count line) 2))
               (- row (quot (count %) 2))
               c])
            line)) %))
    (flatten)
    (partition 3)
    (reduce (fn [acc [x y c]] (if (= \# c) (conj acc [x y]) acc)) #{})
    (assoc {} :counter 0 :dirs [[N NE NW] [S SE SW] [W NW SW] [E NE SE]] :board)
    (iterate move)
    ((fn [move-iterations]
       (let [part-1-solution (count-empty (nth move-iterations 10))
             first-equal-boards (->> move-iterations (partition 2 1) (filter #(= (:board (first %)) (:board (second %)))) (first))
             part-2-solution (:counter (second first-equal-boards))]
         [part-1-solution part-2-solution])))))
