(def input
  "30373
25512
65332
33549
35390")

(defn visible? [row column field]
  (let [top-ones (map #(get-in field [% column]) (range row))
        bottom-ones (map #(get-in field [% column]) (range (inc row) (count field)))
        left-ones (map #(get-in field [row %]) (range column))
        right-ones (map #(get-in field [row %]) (range (inc column) (count (first field))))
        tree-hight (get-in field [row column])
        higher-tree? #(>= % tree-hight)]
    (some #(not-any? higher-tree? %) [top-ones right-ones bottom-ones left-ones])))
    ;; [top-ones right-ones bottom-ones left-ones]))
    
;; Part One
(let [field (->> input
              (clojure.string/split-lines)
              (map #(re-seq #"\d" %))
              (mapv (fn [r] (mapv #(Integer/parseInt %) r))))]
  (count
    (filter true?
      (for [i (range (count field))
            j (range (count (first field)))]
        (visible? i j field)))))
  ;; (visible? 0 1 field))

(defn scenic-score [row column field]
  (let [top-ones (reverse (map #(get-in field [% column]) (range row)))
        bottom-ones (map #(get-in field [% column]) (range (inc row) (count field)))
        left-ones (reverse (map #(get-in field [row %]) (range column)))
        right-ones (map #(get-in field [row %]) (range (inc column) (count (first field))))
        tree-hight (get-in field [row column])
        lower-tree? #(< % tree-hight)
        take-visibles (fn [trees]
                        (let [[lowers same-or-highers] (split-with lower-tree? trees)]
                          (remove nil? (conj (vec lowers) (first same-or-highers)))))]
    (->> [top-ones right-ones bottom-ones left-ones]
      (map take-visibles)
      (map count)
      (apply *))))
    
;; Part Two
(let [field (->> input
              (clojure.string/split-lines)
              (map #(re-seq #"\d" %))
              (mapv (fn [r] (mapv #(Integer/parseInt %) r))))]
  (apply max
    (for [i (range (count field))
          j (range (count (first field)))]
      (scenic-score i j field))))
  ;; (scenic-score 1 2 field))
