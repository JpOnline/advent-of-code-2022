(require '[clojure.string :as string])

(def input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

;; Part One
(->> input
  (string/split-lines)
  (map #(re-find #"(\d+)-(\d+),(\d+)-(\d+)" %))
  (map (fn [[_ a1 b1 a2 b2]]
         (let [r1 (set (range (Integer/parseInt a1) (inc (Integer/parseInt b1))))
               r2 (set (range (Integer/parseInt a2) (inc (Integer/parseInt b2))))
               inter-count (count (clojure.set/intersection r1 r2))]
           (when (or (= inter-count (count r1))
                     (= inter-count (count r2)))
             :ok))))
  (filter #{:ok})
  (count))

;; Part Two
(->> input
  (string/split-lines)
  (map #(re-find #"(\d+)-(\d+),(\d+)-(\d+)" %))
  (map (fn [[_ a1 b1 a2 b2]]
         (let [r1 (set (range (Integer/parseInt a1) (inc (Integer/parseInt b1))))
               r2 (set (range (Integer/parseInt a2) (inc (Integer/parseInt b2))))
               inter-count (count (clojure.set/intersection r1 r2))]
           (when (> inter-count 0)
             :ok))))
  (filter #{:ok})
  (count))
