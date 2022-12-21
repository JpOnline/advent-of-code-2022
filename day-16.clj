(def input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  Valve BB has flow rate=13; tunnels lead to valves CC, AA
  Valve CC has flow rate=2; tunnels lead to valves DD, BB
  Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
  Valve EE has flow rate=3; tunnels lead to valves FF, DD
  Valve FF has flow rate=0; tunnels lead to valves EE, GG
  Valve GG has flow rate=0; tunnels lead to valves FF, HH
  Valve HH has flow rate=22; tunnel leads to valve GG
  Valve II has flow rate=0; tunnels lead to valves AA, JJ
  Valve JJ has flow rate=21; tunnel leads to valve II")

;; Part One

;; The score function
(defn released-pressure [valves-map result]
  (loop [last-valve nil
         rate 0
         released 0
         valves result]
    (let [in (first valves)]
      (cond
        (empty? valves)
        released

        (= in last-valve)
        (recur
          in
          (+ rate (:rate (valves-map in)))
          (+ released rate)
          (rest valves))
        
        :else
        (recur
          in
          rate
          (+ released rate)
          (rest valves))))))

(defn possible-actions [valves-map visited]
  (let [in (last visited)
        in-opened? (some #{[in in]} (partition 2 1 visited))
        zero-rate? (zero? (:rate (valves-map in))) 
        previous (last (butlast visited))
        to (remove #{previous} (:to (valves-map in)))]
    (if (or in-opened? zero-rate?) 
      to
      (conj to in))))

(defn expand-possibilities [valves-map]
  (fn [r {:keys [visited]}]
    ;; (println visited)
    (into r (map #(into {:visited (conj visited %)}) (possible-actions valves-map visited)))))

(->> input
  (clojure.string/split-lines)
  (reduce (fn [r s]
            (conj r
              {(re-find #"[A-Z][A-Z]" s) {:rate (Integer/parseInt (re-find #"\d+" s))
                                          :to (rest (re-seq #"[A-Z][A-Z]" s))}}))
          {})
  ((fn [valves-map]
     (loop [possible-results [{:visited ["AA"]}]]
       (let [sorted-results (sort-by #(- (released-pressure valves-map (:visited %))) possible-results)
             new-possible-results (take 1000 (reduce (expand-possibilities valves-map) [] sorted-results))]
         (println (count possible-results))
         (cond
           (>= (count (:visited (first possible-results))) 31)
           [valves-map (-> sorted-results first :visited)]

           :else
           (recur new-possible-results))))))
  (apply released-pressure))
