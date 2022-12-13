(def input
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn right-order? [[left & r-left] [right & r-right]]
  (cond
    (and (nil? left) (nil? right)) nil
    (nil? left) true
    (nil? right) false

    (and (vector? left) (vector? right))
    (if (nil? (right-order? left right))
      (right-order? r-left r-right)
      (right-order? left right))

    (and (integer? left) (vector? right))
    (right-order? (cons [left] r-left) (cons right r-right))

    (and (vector? left) (integer? right))
    (right-order? (cons left r-left) (cons [right] r-right))

    (= left right) (right-order? r-left r-right)

    (and (integer? left) (integer? right))
    (< left right)

    :else (println (str "BUG: "left" "right))))

;; Part One
(->> input
  (clojure.string/split-lines)
  (partition-by clojure.string/blank?)
  (remove #{[""]})
  (map #(map edn/read-string %))
  ;; (map #(right-order? %1 %2)))
  (map-indexed (fn [idx [v1 v2]]
                 (if (right-order? v1 v2)
                   (inc idx)
                   0)))
  (apply +))

;; Part Two
(->> input
  (clojure.string/split-lines)
  (remove #{""})
  (map edn/read-string)
  (#(conj % [[2]] [[6]]))
  ((fn [lists]
     (loop [sorted []
            to-sort lists]
       (if (empty? to-sort)
         sorted
         (let [smaller (reduce (fn [v1 v2] (if (right-order? v1 v2) v1 v2)) to-sort)
               new-sorted (conj sorted smaller)
               new-to-sort (remove #{smaller} to-sort)]
           (recur new-sorted new-to-sort))))))
  (map-indexed #(if (#{[[2]] [[6]]} %2) (inc %1) 1))
  (apply *))
