(def input "2,2,2
  1,2,2
  3,2,2
  2,1,2
  2,3,2
  2,2,1
  2,2,3
  2,2,4
  2,2,6
  1,2,5
  3,2,5
  2,1,5
  2,3,5")

;; Part One
(->> input
  (clojure.string/split-lines)
  (map #(map (fn [n] (Integer/parseInt n)) (re-seq #"-?\d+" %)))
  ((fn [cubes] (map (fn [cube]
                      (count
                        (remove nil?
                          (map (fn [dir]
                                 ((set cubes) (map + cube dir)))
                               [[-1 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))))
                    cubes)))
  (#(- (* 6 (count %)) (apply + %))))


;; Part Two

(defn adjacents [cubes]
  (for [dir [[-1 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
        cube cubes]
    (map + dir cube)))

;; The idea here was to surround the whole strucuture by "walls" in all dimensions
;; represented by the max-[x,y,z] and create virtual adjacent cubes starting by a
;; cube's face. If a virtual cube touches the "walls" it's exterior and if it
;; it's not possible to create more virtual cubes it's interior.
(defn is-exterior? [face cubes-map]
  (let [max-x (apply max (map first (keys cubes-map)))
        max-y (apply max (map second (keys cubes-map)))
        max-z (apply max (map #(nth % 2) (keys cubes-map)))]
    (loop [expanded #{face}]
      (let [exp-adjacents (->> expanded
                            (adjacents)
                            (remove #(expanded %))
                            (remove #(cubes-map %)))
            new-expanded (into expanded exp-adjacents)]
        (cond
          (or (some #(> (first %) max-x) exp-adjacents)
              (some #(> (second %) max-y) exp-adjacents)
              (some #(> (nth % 2) max-z) exp-adjacents))
          true

          (= expanded new-expanded)
          false

          :else
          (recur new-expanded))))))

;; pmap in this case actually reduced computation from 115 to 26 seconds.
(->> input
  (clojure.string/split-lines)
  (map #(into {(map (fn [n] (Integer/parseInt n)) (re-seq #"-?\d+" %)) true}))
  (apply merge)
  ((fn [cubes-map]
     (->> (keys cubes-map)
       (adjacents)
       (remove #(cubes-map %))
       (filter #(is-exterior? % cubes-map))
       ;; (pmap #(is-exterior? % cubes-map))
       ;; (remove false?)
       (count)))))
