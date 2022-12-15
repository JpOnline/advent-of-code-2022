(def input
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

;; Part One
(->> input
  (partition 4 1)
  (map-indexed #(into [(+ 4 %1) (= 4 (count (distinct %2)))]))
  (filter #(true? (second %)))
  (first))

;; Part Two
(->> input
  (partition 14 1)
  (map-indexed #(into [(+ 14 %1) (= 14 (count (distinct %2)))]))
  (filter #(true? (second %)))
  (first))
