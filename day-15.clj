(do
  (def desired-y 10) ;; For part One
  (def search-space 20) ;; For part Two
  (def input
      "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
  Sensor at x=9, y=16: closest beacon is at x=10, y=16
  Sensor at x=13, y=2: closest beacon is at x=15, y=3
  Sensor at x=12, y=14: closest beacon is at x=10, y=16
  Sensor at x=10, y=20: closest beacon is at x=10, y=16
  Sensor at x=14, y=17: closest beacon is at x=10, y=16
  Sensor at x=8, y=7: closest beacon is at x=2, y=10
  Sensor at x=2, y=0: closest beacon is at x=2, y=10
  Sensor at x=0, y=11: closest beacon is at x=2, y=10
  Sensor at x=20, y=14: closest beacon is at x=25, y=17
  Sensor at x=17, y=20: closest beacon is at x=21, y=22
  Sensor at x=16, y=7: closest beacon is at x=15, y=3
  Sensor at x=14, y=3: closest beacon is at x=15, y=3
  Sensor at x=20, y=1: closest beacon is at x=15, y=3"))

(do
  (def desired-y 2000000) ;; For part One
  (def search-space 4000000) ;; For part Two
  (def input
      "Sensor at x=193758, y=2220950: closest beacon is at x=652350, y=2000000
  Sensor at x=3395706, y=3633894: closest beacon is at x=3404471, y=3632467
  Sensor at x=3896022, y=3773818: closest beacon is at x=3404471, y=3632467
  Sensor at x=1442554, y=1608100: closest beacon is at x=652350, y=2000000
  Sensor at x=803094, y=813675: closest beacon is at x=571163, y=397470
  Sensor at x=3491072, y=3408908: closest beacon is at x=3404471, y=3632467
  Sensor at x=1405010, y=486446: closest beacon is at x=571163, y=397470
  Sensor at x=3369963, y=3641076: closest beacon is at x=3404471, y=3632467
  Sensor at x=3778742, y=2914974: closest beacon is at x=4229371, y=3237483
  Sensor at x=1024246, y=3626229: closest beacon is at x=2645627, y=3363491
  Sensor at x=3937091, y=2143160: closest beacon is at x=4229371, y=3237483
  Sensor at x=2546325, y=2012887: closest beacon is at x=2645627, y=3363491
  Sensor at x=3505386, y=3962087: closest beacon is at x=3404471, y=3632467
  Sensor at x=819467, y=239010: closest beacon is at x=571163, y=397470
  Sensor at x=2650614, y=595151: closest beacon is at x=3367919, y=-1258
  Sensor at x=3502942, y=6438: closest beacon is at x=3367919, y=-1258
  Sensor at x=3924022, y=634379: closest beacon is at x=3367919, y=-1258
  Sensor at x=2935977, y=2838245: closest beacon is at x=2645627, y=3363491
  Sensor at x=1897626, y=7510: closest beacon is at x=3367919, y=-1258
  Sensor at x=151527, y=640680: closest beacon is at x=571163, y=397470
  Sensor at x=433246, y=1337298: closest beacon is at x=652350, y=2000000
  Sensor at x=2852855, y=3976978: closest beacon is at x=3282750, y=3686146
  Sensor at x=3328398, y=3645875: closest beacon is at x=3282750, y=3686146
  Sensor at x=3138934, y=3439134: closest beacon is at x=3282750, y=3686146
  Sensor at x=178, y=2765639: closest beacon is at x=652350, y=2000000
  Sensor at x=3386231, y=3635056: closest beacon is at x=3404471, y=3632467
  Sensor at x=3328074, y=1273456: closest beacon is at x=3367919, y=-1258
  Sensor at x=268657, y=162438: closest beacon is at x=571163, y=397470"))

(defn distance [[sx sy :as _s] [bx by :as _b]]
  (+ (abs (- sx bx)) (abs (- sy by))))

(defn can-influence-desired-y? [desired-y [[_sx sy :as s] b :as _pair]]
  (<= (- sy (distance s b)) desired-y (+ sy (distance s b))))

(defn find-xs-with-no-beacon [desired-y]
  (fn [[[sx _sy :as s] [_bx _by :as b]]]
    (let [distance-to-desired-y (distance s [sx desired-y])
          spare-distance (- (distance s b) distance-to-desired-y)
          left-boundary (- sx spare-distance)
          right-boundary (+ sx spare-distance 1)]
      ;; (print (str "["left-boundary" "right-boundary"]"))
      (range (if (neg? left-boundary) 0 left-boundary)
             (if (> right-boundary search-space) (inc search-space) right-boundary)))))

(defn beacon-boundaries [desired-y]
  (fn [[[sx _sy :as s] [_bx _by :as b]]]
    (let [distance-to-desired-y (distance s [sx desired-y])
          spare-distance (- (distance s b) distance-to-desired-y)
          left-boundary (- sx spare-distance)
          right-boundary (+ sx spare-distance 1)]
      [left-boundary right-boundary])))

(->> input
  ;; Parsing input
  (re-seq #"-?\d+")
  (map #(Integer/parseInt %))
  (partition 2)
  (partition 2)
  
  ;; ;; Checking distance calculation
  ;; (map (fn [[[sx sy :as s] [bx by :as b]]]
  ;;        [s b (+ (abs (- sx bx)) (abs (- sy by)))]))
  ;; (sort-by #(nth % 2)))

  ;; ;; Part One solution
  ;; (filter #(can-influence-desired-y? desired-y %))
  ;; (mapcat (fn [[[sx sy :as s] [bx by :as b]]]
  ;;           ;; [s b (distance s [sx desired-y]) (distance s b)]
  ;;           (let [distance-to-desired-y (distance s [sx desired-y])
  ;;                 spare-distance (- (distance s b) distance-to-desired-y)
  ;;                 xs-with-no-beacon (range (- sx spare-distance) (+ sx spare-distance 1))]
  ;;             (if (= by desired-y)
  ;;               (remove #{bx} xs-with-no-beacon)
  ;;               xs-with-no-beacon))))
  ;; (set)
  ;; (count))


  ;; ;; Part Two naive attempt, simply applying a similar logic of Part One solution.
  ;; ((fn [pairs]
  ;;    (some (fn [search-y]
  ;;            (let [testing-pairs (filter #(can-influence-desired-y? search-y %) pairs)
  ;;                  xs-with-no-beacon (set (mapcat (find-xs-with-no-beacon search-y) testing-pairs))
  ;;                  min-xs (apply min xs-with-no-beacon)
  ;;                  max-xs (apply max xs-with-no-beacon)
  ;;                  get-x #(first (clojure.set/difference
  ;;                                  (set (range min-xs (inc max-xs)))
  ;;                                  xs-with-no-beacon))]
  ;;              (println search-y)
  ;;              (println (str "["min-xs" "max-xs"] count: "(count xs-with-no-beacon)))
  ;;              (println)
  ;;              (when (not= (dec (count xs-with-no-beacon))
  ;;                          (- max-xs min-xs))
  ;;                [(get-x) search-y])))
  ;;          (range (inc search-space)))))
  ;; ((fn [[x y]] (+ (* x 4000000) y))))

  ;; Part Two final solution
  ((fn [pairs]
     (some (fn [search-y]
             (let [testing-pairs (filter #(can-influence-desired-y? search-y %) pairs)
                   boundaries (sort (map (beacon-boundaries search-y) testing-pairs))
                   max-right-final (apply max (map second boundaries))
                   last-continuous (reduce (fn [lc [ri rf]]
                                             (if (> lc (dec ri))
                                               (max lc rf)
                                               lc))
                                           0
                                           boundaries)]
               ;; (println search-y)
               ;; (println (count boundaries))
               ;; (println (sort boundaries))
               ;; (println last-continuous)
               ;; (println max-right-final)
               ;; (println (str "["min-xs" "max-xs"] count: "(count xs-with-no-beacon)))
               ;; (println)
               (when (not= last-continuous max-right-final)
                 [last-continuous search-y])))
           (range (inc search-space)))))
  ((fn [[x y]] (+ (* x 4000000) y))))

  ;; ;; Part One attempt. This one is right, the performance was not that bad, but unfortunately
  ;; ;; I was using the wrong `desired-y` and I thought the solution was wrong 😞
  ;; ((fn [pairs]
  ;;    (filter
  ;;      (fn a [x]
  ;;        ;; (->> pairs)
  ;;          ;; (map (fn [[s b]] [x (<= (distance [x desired-y] s) (distance s b)) s b]))))
  ;;        (some (fn [[s b]] (<= (distance [x desired-y] s) (distance s b))) pairs))
  ;;      (apply range (range-x-of-interest pairs)))))
  ;; (count)
  ;; (dec))

  ;; ;; Part One attempt. This performance is horrible, it wasn't able to give me the answer..
  ;; (reduce (fn [board [[sx sy] [bx by]]]
  ;;           (let [distance (+ (abs (- sx bx)) (abs (- sy by)))
  ;;                 empties (for [dir [[1 -1] [1 1] [-1 1] [-1 -1]]
  ;;                               x (range (inc distance))
  ;;                               y (range (inc distance))
  ;;                               :when (<= (+ x y) distance)]
  ;;                           {(mapv + [sx sy] (map * dir [x y])) \#})]
  ;;             (merge (into {} empties)
  ;;                    board
  ;;                    {[bx by] \B
  ;;                     [sx sy] \S})))
  ;;
  ;;         {}))
  ;; ;; (filter (fn [[[_x y] v]] (and (= 2000000 y) (= \# v))))
  ;; ;; (sort-by ffirst))
  ;; ;; (count))

(comment
  (some #(when (even? %) %) [1 3 5 8 9])
  (time (some #(when (= 40000000 (count (range %))) %) (range)))
  (time (some #(when (= 40000000 %) %) (pmap #(count (range %)) (range))))
  
  (def range-x-of-interest
    (memoize
      (fn [pairs]
        (let [horizontal-ranges (map (fn [[[sx _sy :as s] [_bx _by :as b]]]
                                       [(- sx (distance s b)) (+ sx (distance s b))])
                                     pairs)]
          [(apply min (map first horizontal-ranges))
           (apply max (map second horizontal-ranges))])))))
