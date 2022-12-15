(def input)
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

(require '[clojure.string :as string])
(->> input
  (re-seq #"(\d*)\n")
  (map second)
  (map #(if (string/blank? %) nil (Integer/parseInt %)))
  (partition-by nil?)
  (remove #{'(nil)})
  (map #(apply + %))
  (apply max))

(->> input
  (re-seq #"(\d*)\n{0,1}")
  (map second)
  (map #(if (string/blank? %) nil (Integer/parseInt %)))
  (partition-by nil?)
  (remove #{'(nil)})
  (map #(apply + %))
  (sort)
  (reverse)
  (take 3)
  (apply +))

