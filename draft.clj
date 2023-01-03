
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
  (apply min))

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
  (apply +)

  (map #(apply + %) (remove #(every? string/blank? %) (partition-by string/blank? (map second (re-seq #"(\d*)\n" input)))))
  (or (Integer/parseInt "") 0))

(def input
  "A Y
B X
C Z")

;; Part 1
(let [win? #{[\A \Y] [\B \Z] [\C \X]}
      choice-points {\X 1 \Y 2 \Z 3}
      draw? #{[\A \X] [\B \Y] [\C \Z]}
      games (map #(into [(first %) (nth % 2)]) (re-seq #"[ABC]\s[XYZ]\n{0,1}" input))]
  (+
   (apply + (map #(choice-points (second %)) games))
   (* 3 (count (filter draw? games)))
   (* 6 (count (filter win? games)))))

;; Part 2
(let [win? #{[\A \Y] [\B \Z] [\C \X]}
      choice-points {\X 1 \Y 2 \Z 3}
      draw? #{[\A \X] [\B \Y] [\C \Z]}
      games (map #(into [(first %) (nth % 2)]) (re-seq #"[ABC]\s[XYZ]\n{0,1}" input))
      new-choice-lookup {[\A \X] \Z [\A \Y] \X [\A \Z] \Y [\B \X] \X [\B \Y] \Y [\B \Z] \Z [\C \X] \Y [\C \Y] \Z [\C \Z] \X}
      updated-games (map #(into [(first %) (new-choice-lookup %)]) games)]
  (+
   (apply + (map #(choice-points (second %)) updated-games))
   (* 3 (count (filter draw? updated-games)))
   (* 6 (count (filter win? updated-games)))))

(apply + [1 2] [3 4])

;; Day 8

(def input1
  "30373
25512
65332
33549
35390")

(def input2
  "")

(->> (iterate #(mapv + [-1 0] %) [2 2])
  (map #(get-in field %))
  (take-while some?)
  (rest))

(defn visible? [row column field]
  (let [top-ones    (map #(get-in field [% column]) (range row))
        bottom-ones (map #(get-in field [% column]) (range (inc row)    (count field)))
        left-ones   (map #(get-in field [row %])    (range column))
        right-ones  (map #(get-in field [row %])    (range (inc column) (count (first field))))
        tree-hight (get-in field [row column])
        higher-tree? #(>= % tree-hight)]
    (some #(not-any? higher-tree? %) [top-ones right-ones bottom-ones left-ones])))
    ;; [top-ones right-ones bottom-ones left-ones]))
    
;; Another option. More readable? 🤷
(defn visible?-option-2 [row column field]
  (let [get-trees (fn [direction]
                    (->> [row column]
                      (iterate #(mapv + direction %))
                      (map #(get-in field %))
                      (take-while some?)
                      (rest)))
        directions [[0 -1] [1 0] [0 1] [-1 0]]
        trees-in-directions (map get-trees directions)
        tree-hight (get-in field [row column])
        higher-tree? #(>= % tree-hight)]
    (some #(not-any? higher-tree? %) trees-in-directions)))
    
;; Part One
(let [field (->> input
              (string/split-lines)
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
              (string/split-lines)
              (map #(re-seq #"\d" %))
              (mapv (fn [r] (mapv #(Integer/parseInt %) r))))]
  (apply max
    (for [i (range (count field))
          j (range (count (first field)))]
      (scenic-score i j field))))
  ;; (scenic-score 1 2 field))

;; Day 9

(def input1
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def input1
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def input2
  "")

(def dir
  {"R" [1 0]
   "U" [0 1]
   "L" [-1 0]
   "D" [0 -1]})

;;;; Head - Tail
;; (map - [0 3] [2 2])

;; n222n.
;; 21112.
;; 21T12.
;; 21112.
;; n222n.

(def tail-dir-by-diff
  {[-2 2] [-1 1] ;; included in part two
   [-1 2] [-1 1]
   [0 2] [0 1]
   [1 2] [1 1]
   [2 2] [1 1] ;; included in part two
   [-2 1] [-1 1]
   [-1 1] [0 0]
   [0 1] [0 0]
   [1 1] [0 0]
   [2 1] [1 1]
   [-2 0] [-1 0]
   [-1 0] [0 0]
   [0 0] [0 0]
   [1 0] [0 0]
   [2 0] [1 0]
   [-2 -1] [-1 -1]
   [-1 -1] [0 0]
   [0 -1] [0 0]
   [1 -1] [0 0]
   [2 -1] [1 -1]
   [-2 -2] [-1 -1] ;; included in part two
   [-1 -2] [-1 -1]
   [0 -2] [0 -1]
   [1 -2] [1 -1]
   [2 -2] [1 -1]}) ;; included in part two

;; Part One

(require '[clojure.string :as string])
(->> input
  (string/split-lines)
  (map #(re-find #"(.) (\d+)" %))
  (map #(into [(second %) (Integer/parseInt (nth % 2))]))
  (map #(repeat (second %) (dir (first  %))))
  (flatten)
  (partition 2)
  (reduce
    (fn [{:keys [head-positions tail-positions]} head-move-dir]
      (let [move #(map + %1 %2)
            new-head-position (move (last head-positions) head-move-dir)
            head-diff-tail (map - new-head-position (last tail-positions))
            tail-move-dir (tail-dir-by-diff head-diff-tail)
            new-tail-position (move (last tail-positions) tail-move-dir)]
        {:head-positions (conj head-positions new-head-position)
         :tail-positions (conj tail-positions new-tail-position)}))
    {:head-positions [[0 0]]
     :tail-positions [[0 0]]})
  (:tail-positions)
  (distinct)
  (count))

(repeat 10 [[0 0]])

(def move #(map + %1 %2))

(defn move-knots [head-knot-positions tail-positions]
  (let [head-position (last (last head-knot-positions))
        tail-position (last tail-positions)
        head-diff-tail (map - head-position tail-position)
        tail-move-dir (tail-dir-by-diff head-diff-tail)
        new-tail-position (move tail-position tail-move-dir)
        new-tail-positions (conj tail-positions new-tail-position)]
    (conj head-knot-positions new-tail-positions)))

(get (vec (range 10)) 9)

;; Part Two
(->> input
  (string/split-lines)
  (map #(re-find #"(.) (\d+)" %))
  (map #(into [(second %) (Integer/parseInt (nth % 2))]))
  (map #(repeat (second %) (dir (first  %))))
  (flatten)
  (partition 2)
  (reduce
    (fn [knots-positions head-move-dir]
      (let [new-head-position (move (last (first knots-positions)) head-move-dir)
            new-head-positions (conj (first knots-positions) new-head-position)]
        (reduce move-knots [new-head-positions] (rest knots-positions))))
    (vec (repeat 10 [[0 0]])))
  ;; (map-indexed #(into {%1 (last %2)})))
  ;; (map count)
  (#(nth % 9))
  (distinct)
  (count))

;; Day 10

(def input1
  "")

(def input2
  "")

(require '[clojure.string :as string])

;; Part One
(->> input
  (string/split-lines)
  (map #(cond
          (re-find #"noop" %)
          [1 0]
          (re-find #"addx" %)
          [2 (Integer/parseInt (re-find #"-?\d+" %))]
          :else nil))
  (reduce (fn [x-values [cycles parcel]] 
            (if (= 1 cycles)
              (conj x-values (last x-values))
              (conj (conj x-values (last x-values)) (+ (last x-values) parcel))))
          [0 1])
  ;; (rest)
  ((fn [x-values] (map #(* % (nth x-values %)) (range 20 221 40))))
  (apply +))

;; Part Two
(->> input
  (string/split-lines)
  (map #(cond
          (re-find #"noop" %)
          [1 0]
          (re-find #"addx" %)
          [2 (Integer/parseInt (re-find #"-?\d+" %))]
          :else nil))
  (reduce (fn [x-values [cycles parcel]] 
            (if (= 1 cycles)
              (conj x-values (last x-values))
              (conj (conj x-values (last x-values)) (+ (last x-values) parcel))))
          [1])
  (reduce (fn [[idx crt] sprite-pos]
            (if (<= (dec sprite-pos) idx (inc sprite-pos))
              [(if (= 39 idx) 0 (inc idx)) (str crt "#")]
              [(if (= 39 idx) 0 (inc idx)) (str crt ".")]))
          [0 ""])
  (second)
  (partition 40)
  (map #(apply str %)))
  
;; Day 11

(def input1
  "")

(def input2
  "")

(require '[clojure.string :as string]

;; Part One
  (defn throw-an-item-factory [{:keys [operation test if-true if-false]}]
    (fn [monkeys item]
      (let [[op op-num] operation
            op (if (= "*" op) * +)
            op-num (if (= "old" op-num)
                     item
                     (Integer/parseInt op-num))
            new-worry-level (int (Math/floor (/ (op item op-num) 3)))
            monkey-destiny (if (zero? (mod new-worry-level test))
                             if-true
                             if-false)]
        (update monkeys monkey-destiny update :items conj new-worry-level))))

  (def monkey-stats (atom {}))

  (defn process-round [monkeys]
    (loop [idx 0
           monkeys monkeys]
      (if (nil? (get monkeys idx))
        monkeys
        (let [cur-monkey (get monkeys idx)
              {:keys [items if-false]} cur-monkey
              throw-an-item (throw-an-item-factory cur-monkey)
              new-monkeys (reduce throw-an-item monkeys items)
              new-monkeys (update new-monkeys idx assoc :items [])]
          (swap! monkey-stats #(merge-with + % {idx (count items)}))
          (recur (inc idx) new-monkeys)))))

  (->> input
    (string/split-lines)
    (partition-by #(= "" %))
    (remove #{[""]})
    (map-indexed (fn [idx monkey-satus]
                   (let [items (map #(Integer/parseInt %) (re-seq #"\d+" (nth monkey-satus 1)))
                         operation (if-let [r (re-find #"Operation: new = old ([\*\+]) ((?:old)|(?:\d+))" (nth monkey-satus 2))]
                                     (rest r)
                                     (throw (ex-info "different operation" {:bla "x"})))
                         testing (if-let [r (re-find #"divisible by (\d+)" (nth monkey-satus 3))]
                                   (Integer/parseInt (nth r 1))
                                   (throw (ex-info "different test" {:idx idx})))
                         if-true (if-let [r (re-find #"If true: throw to monkey (\d+)" (nth monkey-satus 4))]
                                   (Integer/parseInt (nth r 1))
                                   (throw (ex-info "different true" {:idx idx})))
                         if-false (if-let [r (re-find #"If false: throw to monkey (\d+)" (nth monkey-satus 5))]
                                    (Integer/parseInt (nth r 1))
                                    (throw (ex-info "different false" {:idx idx})))]
                         
                     {:items (vec items)
                      :operation operation
                      :test testing
                      :if-true if-true
                      :if-false if-false})))
    (vec)
    (iterate process-round)
    (#(nth % 20)))

  (->> @monkey-stats
    (vals)
    (sort)
    (reverse)
    (take 2)
    (apply *)))

;; Part two

(require '[clojure.core.memoize :as memo])

;; (def m-mod (memo/memo mod))
;; (def m-* (memo/memo *))
;; (def m-+ (memo/memo +))

(defn monkey-throw-original [{:keys [operation test if-true if-false]} item]
  (let [[op op-num] operation
        op (if (= "*" op) * +)
        op-num (if (= "old" op-num)
                 item
                 (Integer/parseInt op-num))
        _ (println "Calculating worry")
        new-worry-level (bigint (op item op-num))
        _ (println (str "Calculating destiny with mod "test))
        monkey-destiny (if (zero? (mod new-worry-level test))
                         if-true
                         if-false)]
    [monkey-destiny new-worry-level]))

(def monkey-throw (memo/memo monkey-throw-original))

(defn new-throw-an-item-factory [monkey]
  (fn [monkeys item]
    (let [[monkey-destiny new-worry-level] (monkey-throw (dissoc monkey :items) item)]
      (update monkeys monkey-destiny update :items conj new-worry-level))))

(def monkey-stats (atom {}))

(defn t1 [r {:keys [items] :as monkey}]
  (concat r (map #(into [(dissoc monkey :items) %]) items)))

(defn process-round [monkeys]
  (loop [idx 0
         monkeys monkeys]
    (if (nil? (get monkeys idx))
      monkeys
      (let [cur-monkey (get monkeys idx)
            {:keys [items if-false]} cur-monkey
            throw-an-item (new-throw-an-item-factory cur-monkey)
            _ (pmap monkey-throw (reduce t1 [] monkeys))
            new-monkeys (reduce throw-an-item monkeys items)
            new-monkeys (update new-monkeys idx assoc :items [])]
        (println (str "processed monkey "idx))
        (swap! monkey-stats #(merge-with + % {idx (count items)}))
        (recur (inc idx) new-monkeys)))))

(->> input
  (string/split-lines)
  (partition-by #(= "" %))
  (remove #{[""]})
  (map-indexed (fn [idx monkey-satus]
                 (let [items (map #(Integer/parseInt %) (re-seq #"\d+" (nth monkey-satus 1)))
                       operation (if-let [r (re-find #"Operation: new = old ([\*\+]) ((?:old)|(?:\d+))" (nth monkey-satus 2))]
                                   (rest r)
                                   (throw (ex-info "different operation" {:bla "x"})))
                       testing (if-let [r (re-find #"divisible by (\d+)" (nth monkey-satus 3))]
                                 (Integer/parseInt (nth r 1))
                                 (throw (ex-info "different test" {:idx idx})))
                       if-true (if-let [r (re-find #"If true: throw to monkey (\d+)" (nth monkey-satus 4))]
                                 (Integer/parseInt (nth r 1))
                                 (throw (ex-info "different true" {:idx idx})))
                       if-false (if-let [r (re-find #"If false: throw to monkey (\d+)" (nth monkey-satus 5))]
                                  (Integer/parseInt (nth r 1))
                                  (throw (ex-info "different false" {:idx idx})))]
                       
                   {:items (vec items)
                    :operation operation
                    :test testing
                    :if-true if-true
                    :if-false if-false})))
  (vec)
  ;; (iterate process-round)
  ;; (#(nth % 100000)))
  (#(let [monkeys (atom %)]
      ;; @monkeys)))
      (dotimes [n 10000]
        (println)
        (println n)
        (when (= 0 (mod n 5))
          (do
            (memo/memo-clear! monkey-throw)))
            ;; (memo/memo-clear! m-mod)
            ;; (memo/memo-clear! m-*)
            ;; (memo/memo-clear! m-+)))
        (time (swap! monkeys process-round))))))
      ;; @monkeys)))

(->> @monkey-stats
  (vals)
  (sort)
  (reverse)
  (take 2)
  (apply *))

(first @monkey-stats)

(conj '(1 2 3) 0)
(get '(1 2 3 1) 1)

;; Day 16

(do
  (def input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"))

(do
  (def input "Valve RT has flow rate=0; tunnels lead to valves EN, LZ
Valve VB has flow rate=0; tunnels lead to valves SZ, BF
Valve AD has flow rate=0; tunnels lead to valves EB, JF
Valve RE has flow rate=4; tunnels lead to valves QB, IF, XT, WF, KW
Valve RL has flow rate=0; tunnels lead to valves DQ, LZ
Valve OK has flow rate=0; tunnels lead to valves QH, BF
Valve RV has flow rate=0; tunnels lead to valves IU, JF
Valve TE has flow rate=0; tunnels lead to valves HE, XF
Valve WW has flow rate=0; tunnels lead to valves QH, YZ
Valve HB has flow rate=15; tunnel leads to valve OM
Valve IY has flow rate=14; tunnels lead to valves UH, KW, BN, LW, UY
Valve QF has flow rate=0; tunnels lead to valves JF, PL
Valve YZ has flow rate=0; tunnels lead to valves JG, WW
Valve QB has flow rate=0; tunnels lead to valves SP, RE
Valve SO has flow rate=0; tunnels lead to valves QH, SZ
Valve EB has flow rate=7; tunnels lead to valves IF, NH, AD, VI, DQ
Valve VL has flow rate=0; tunnels lead to valves JF, YV
Valve BF has flow rate=18; tunnels lead to valves OK, VB, OH, SX
Valve UC has flow rate=0; tunnels lead to valves SC, YV
Valve OQ has flow rate=0; tunnels lead to valves XT, AA
Valve YV has flow rate=6; tunnels lead to valves YX, TT, VL, UC, NH
Valve KJ has flow rate=0; tunnels lead to valves OH, JG
Valve QH has flow rate=20; tunnels lead to valves SO, OK, WW
Valve KW has flow rate=0; tunnels lead to valves RE, IY
Valve PL has flow rate=0; tunnels lead to valves JG, QF
Valve DQ has flow rate=0; tunnels lead to valves EB, RL
Valve AA has flow rate=0; tunnels lead to valves YI, EN, UK, OQ, VI
Valve XT has flow rate=0; tunnels lead to valves OQ, RE
Valve SZ has flow rate=24; tunnels lead to valves VB, SO
Valve IU has flow rate=25; tunnels lead to valves RV, HE, HQ
Valve OM has flow rate=0; tunnels lead to valves NY, HB
Valve YX has flow rate=0; tunnels lead to valves YV, SI
Valve SX has flow rate=0; tunnels lead to valves ZB, BF
Valve KD has flow rate=0; tunnels lead to valves XF, LW
Valve SP has flow rate=0; tunnels lead to valves XF, QB
Valve UY has flow rate=0; tunnels lead to valves UK, IY
Valve XF has flow rate=22; tunnels lead to valves SP, TE, KD, NY
Valve SC has flow rate=0; tunnels lead to valves LZ, UC
Valve UK has flow rate=0; tunnels lead to valves UY, AA
Valve LW has flow rate=0; tunnels lead to valves KD, IY
Valve FL has flow rate=0; tunnels lead to valves BN, LZ
Valve VI has flow rate=0; tunnels lead to valves AA, EB
Valve HW has flow rate=0; tunnels lead to valves JF, CY
Valve YI has flow rate=0; tunnels lead to valves AA, TT
Valve HE has flow rate=0; tunnels lead to valves IU, TE
Valve JG has flow rate=10; tunnels lead to valves PL, YZ, SI, KJ
Valve BN has flow rate=0; tunnels lead to valves IY, FL
Valve IF has flow rate=0; tunnels lead to valves EB, RE
Valve JF has flow rate=19; tunnels lead to valves HW, QF, VL, RV, AD
Valve SI has flow rate=0; tunnels lead to valves JG, YX
Valve WF has flow rate=0; tunnels lead to valves LZ, RE
Valve HQ has flow rate=0; tunnels lead to valves IU, UH
Valve LZ has flow rate=5; tunnels lead to valves SC, FL, WF, RL, RT
Valve UH has flow rate=0; tunnels lead to valves IY, HQ
Valve CY has flow rate=21; tunnel leads to valve HW
Valve NH has flow rate=0; tunnels lead to valves EB, YV
Valve TT has flow rate=0; tunnels lead to valves YV, YI
Valve OH has flow rate=0; tunnels lead to valves KJ, BF
Valve EN has flow rate=0; tunnels lead to valves RT, AA
Valve NY has flow rate=0; tunnels lead to valves OM, XF
Valve ZB has flow rate=8; tunnel leads to valve SX")

;; Part One
  (->> result-example
    (clojure.string/split-lines)
    (filter #(re-find #"You" %))
    (map #(re-find #"[A-Z][A-Z]" %)))

  (count (concat r1 ["DD" "CC" "DD" "CC" "DD" "CC"]))
  (released-pressure valves-map r1)
 
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
    (apply released-pressure)))

;; Part Two
      
(defn released-pressure-with-elephant [valves-map [person-result elephant-result]]
  (loop [p-last-valve nil
         e-last-valve nil
         rate 0
         released 0
         p-valves person-result
         e-valves elephant-result]
    (let [p-in (first p-valves)
          e-in (first e-valves)
          new-rate-fn #(if (= %2 %3) (+ %1 (:rate (valves-map %2))) %1)
          new-rate (-> rate
                     (new-rate-fn p-in p-last-valve)
                     (new-rate-fn e-in e-last-valve))]
      (if (empty? p-valves)
        released
        (recur p-in e-in new-rate (+ released rate)
               (rest p-valves) (rest e-valves))))))

(def r2
  (->> elephant-example
    (clojure.string/split-lines)
    ((fn [lines] [(filter #(re-find #"You" %) lines) (filter #(re-find #"elephant" %) lines)]))
    (map (fn [p] (map #(re-find #"[A-Z][A-Z]" %) p)))
    (#(into [(concat (first %) (flatten (repeat 8 ["AA" "BB"])) ["AA"])
             (concat (second %) (flatten (repeat 7 ["AA" "BB"])) ["AA"])]))))

(def r-mine
  [["BB" "BB" "CC" "CC" "DD" "CC" "DD" "AA" "II" "JJ" "JJ" "II" "AA" "DD" "CC" "DD" "CC" "DD" "CC" "DD" "CC" "DD" "CC" "DD" "CC" "DD"]
   ["DD" "DD" "EE" "EE" "FF" "GG" "HH" "HH" "GG" "FF" "EE" "FF" "EE" "FF" "EE" "FF" "EE" "FF" "EE" "FF" "EE" "FF" "EE" "FF" "EE" "FF"]])

(def r-official
  [["II" "JJ" "JJ" "II" "AA" "BB" "BB" "CC" "CC" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA"]
   ["DD" "DD" "EE" "FF" "GG" "HH" "HH" "GG" "FF" "EE" "EE" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA" "BB" "AA"]])

(released-pressure-with-elephant valves-map r-mine)
(released-pressure-with-elephant valves-map r-official)

(defn possible-actions-2 [valves-map [p-visited e-visited]]
  (let [p-in (last p-visited)
        e-in (last e-visited)
        someone-opened? #(or (some #{[% %]} (partition 2 1 p-visited))
                             (some #{[% %]} (partition 2 1 e-visited)))
        zero-rate? #(zero? (:rate (valves-map %))) 
        possibilities #(if (or (someone-opened? %) (zero-rate? %))
                         (:to (valves-map %))
                         (conj (:to (valves-map %)) %))
        e-possibilities (if (= p-in e-in)
                          (remove #{p-in} (possibilities e-in))
                          (possibilities e-in))]
    [(possibilities p-in) e-possibilities]))

(possible-actions-2
  valves-map
  [["DD"]
   ["DD"]])
                    

(defn expand-possibilities-3 [valves-map]
  (fn [r {:keys [p-visited e-visited]}]
    (let [[p-possibilities e-possibilities] (possible-actions-2 valves-map [p-visited e-visited])
          possibilities (for [p p-possibilities
                              e e-possibilities]
                          [p e])]
      (into r (map (fn [[p-valve e-valve]]
                     {:p-visited (conj p-visited p-valve)
                      :e-visited (conj e-visited e-valve)})
                   possibilities)))))

(defn expand-possibilities-2 [valves-map]
  (fn [r {:keys [p-visited e-visited]}]
    ;; (println visited)
    (let [p-possibilities (set (possible-actions valves-map p-visited))
          e-possibilities (set (possible-actions valves-map e-visited))
          opened-by? (fn [ps] #(some #{[% %]} (partition 2 1 ps)))
          e-possibilities (remove (opened-by? p-visited) e-possibilities)
          p-possibilities (remove (opened-by? e-visited) p-possibilities)
          e-possibilities (if (empty? e-possibilities) [(last (butlast e-visited))] e-possibilities)
          p-possibilities (if (empty? p-possibilities) [(last (butlast p-visited))] p-possibilities)
          possibilities (for [p p-possibilities
                              e e-possibilities]
                          [p e])]
      (into r (map (fn [[p-valve e-valve]]
                     {:p-visited (conj p-visited p-valve)
                      :e-visited (conj e-visited e-valve)})
                   possibilities)))))

(map (fn [{:keys [p-visited e-visited]}] [p-visited e-visited (released-pressure-with-elephant valves-map [p-visited e-visited])])
  ((expand-possibilities-3 valves-map)
   []
   {:p-visited ["II" "JJ"]
    :e-visited ["DD" "DD"]}))

((expand-possibilities-3 valves-map)
 []
 {:p-visited ["DD" "DD" "AA" "II" "JJ" "JJ" "II" "AA" "BB" "BB" "CC"]
  :e-visited ["DD" "DD" "AA" "II" "JJ" "JJ" "II" "AA" "BB" "BB" "CC"]})

(distance valves-map "AA" "AA")
;; (defn distance [valves-map v1 v2]
;;   (if ((set (:to (valves-map v1))) v2)
;;     1
;;     (+ 1 (apply min (map #(distance valves-map % v2) (:to (valves-map v1)))))))

(def valves-map
  (->> input
    (clojure.string/split-lines)
    (reduce (fn [r s]
              (conj r
                {(re-find #"[A-Z][A-Z]" s) {:rate (Integer/parseInt (re-find #"\d+" s))
                                            :to (rest (re-seq #"[A-Z][A-Z]" s))}}))
            {})))

(->> valves-map
  (sort-by #(- (:rate (second %)))))

(defn distance-from-good-opened-valves [valves-map [p-visited e-visited]]
  (let [p-opened-valves (set (map first (filter #(apply = %) (partition 2 1 p-visited))))
        e-opened-valves (set (map first (filter #(apply = %) (partition 2 1 e-visited))))
        opened-valves (into p-opened-valves e-opened-valves)]
    (apply +
           (map (fn [v] (- (:rate (valves-map v))
                           (min (distance valves-map (last p-visited) v)
                                (distance valves-map (last p-visited) v))))
                (remove opened-valves (keys valves-map))))))


(distance-from-good-opened-valves
  valves-map
  [["AA" "AA" "DD" "CC"]
   ["DD" "DD" "BB" "GG"]])

(def break (atom 0))
(->> input
  (clojure.string/split-lines)
  (reduce (fn [r s]
            (conj r
              {(re-find #"[A-Z][A-Z]" s) {:rate (Integer/parseInt (re-find #"\d+" s))
                                          :to (rest (re-seq #"[A-Z][A-Z]" s))}}))
          {})
  ((fn [valves-map]
     (loop [possible-results [{:p-visited ["AA"]
                               :e-visited ["AA"]}]]
       ;; (swap! break inc)
       (let [
             sorted-results (sort-by #(- (released-pressure-with-elephant valves-map (vals %))) possible-results)
             ;; sorted-results (take 100 (sort-by #(distance-from-good-opened-valves valves-map (vals %)) possible-results))
             new-possible-results (reduce (expand-possibilities-3 valves-map) [] sorted-results)
             selected-possible-results (take 1000 (sort-by #(- (released-pressure-with-elephant valves-map (vals %))) new-possible-results))]
         ;; (when (> @break 50)
         ;;   (println (first sorted-results)))
         (println (quot (* 100.0 (/ (count (:p-visited (first possible-results))) 27)) 1))
         ;; (println "distance")
         ;; (println (reverse (sort-by :score (map #(assoc % :score (distance-from-good-opened-valves valves-map (vals %))) (take 3 (drop 20 sorted-results))))))
         ;; (println "release")
         ;; (println (reverse (sort-by :score (map #(assoc % :score (released-pressure-with-elephant valves-map (vals %))) (take 3 (drop 20 sorted-results))))))
         ;; (println)
         (cond
           (> @break 500)
           (first sorted-results)

           (>= (count (:p-visited (first possible-results))) 27)
           ;; (->> sorted-results first vals (map #(into (rest %))))
           [valves-map [(-> sorted-results first :p-visited rest) (-> sorted-results first :e-visited rest)]]

           :else
           (recur new-possible-results))))))
  (apply released-pressure-with-elephant))
(clojure.set/union #{1 2} #{})

(do

  (def distance
    (memoize
      (fn [valves-map origin target]
        (loop [distance 0
               visited #{}
               to-visit (:to (valves-map origin))]
               ;; visit-next (map #(:to (valves-map %)) to-visit)]
          (cond
            (= origin target)
            1

            ((set to-visit) target)
            (inc distance)

            :else
            (recur (inc distance)
                   (into visited to-visit)
                   (mapcat #(:to (valves-map %)) to-visit)))))))

  (defn path-to-valve [valves-map origin target]
    (let [to-valves #(:to (valves-map %))]
      (reduce (fn [path _]
                (conj path
                      (if-let [part-of-path (first (filter #(< (distance valves-map % target) (distance valves-map (last path) target))
                                                           (to-valves (last path))))]
                        part-of-path
                        target)))
              [origin]
              (range (distance valves-map origin target)))))

  (distance valves-map "AA" "GG")
  (distance valves-map "FF" "BB")
  (distance valves-map "AA" "HH")
  (get (path-to-valve valves-map "AA" "JJ") 2)
  (get (path-to-valve valves-map "AA" "HH") 2)

  (def counter (atom 0))
  (def solve
    (memoize
      (fn [p-in-valve e-in-valve opened time-left valves-map]
        (let [
              ;; cannot-open-more? (->> (remove #(opened (first %)) valves-map)
              ;;                     (map second)
              ;;                     (map :rate)
              ;;                     (apply +)
              ;;                     (= 0))
              can-open-valve? #(and (not (opened %)) (not (zero? (:rate (valves-map %)))))
              ;; next-valves #(if (can-open-valve? %)
              ;;                (conj (:to (valves-map %)) %)
              ;;                (:to (valves-map %)))
              next-valves (filter can-open-valve? (keys valves-map))
              possibilities (for [p-next next-valves
                                  e-next next-valves
                                  :let [new-opened (cond-> opened
                                                     (= p-in-valve p-next) (conj p-next)
                                                     (= e-in-valve e-next) (conj e-next))]]
                                  ;; :when (not= p-next e-next)]
                              [p-next e-next new-opened])
              pressure-released-by-the-opened (apply + (map :rate (map second (filter #(opened (first %)) valves-map))))]
          ;; (println)
          ;; (println possibilities)
          (cond
            (or (empty? next-valves) (<= time-left 2))
            (* time-left pressure-released-by-the-opened)

            :else
            (do
              (when (= time-left 15)
                (print "A"))
              (when (= time-left 20)
                (print "B"))
              (when (= time-left 24)
                (swap! counter inc)
                (println (quot @counter 0.36)))
              ;; (reduce
              ;;   (fn [m [new-p new-e new-opened]]
              ;;     (let [candidate (+ (solve new-p new-e new-opened (dec time-left) valves-map)
              ;;                        pressure-released-by-the-opened)]
              ;;       (max candidate m)))
              ;;   0 possibilities)))))))
              (apply max (map (fn [[p-target e-target new-opened]]
                                (let [time-spent (min (distance valves-map p-in-valve p-target) (distance valves-map e-in-valve e-target))
                                      new-p (get (path-to-valve valves-map p-in-valve p-target) time-spent)
                                      new-e (get (path-to-valve valves-map e-in-valve e-target) time-spent)]
                                  ;; (println "p-in" p-in-valve "e-in" e-in-valve "p-target" p-target "e-target" e-target "new-p" new-p "new-e" new-e "time-spent" time-spent)
                                  (+ (solve new-p new-e new-opened (- time-left time-spent) valves-map)
                                     pressure-released-by-the-opened)))
                              possibilities))))))))

  (->> input
    (clojure.string/split-lines)
    (reduce (fn [r s]
              (conj r
                {(re-find #"[A-Z][A-Z]" s) {:rate (Integer/parseInt (re-find #"\d+" s))
                                            :to (rest (re-seq #"[A-Z][A-Z]" s))}}))
            {})
    (#(do (def valves-map %) %))
    (#(time (solve "AA" "AA" #{} 26 %)))))

;; Implementing Abyala's solution. In the end it didn't work.
(do
  (require '[clojure.string :as str])
  (defn parse-tunnel [s]
    (let [[_ name rate tunnels] (re-matches #"Valve (\w+) has flow rate=(\d+); .* valves? (.*)" s)
          neighbors (reduce #(assoc %1 %2 1) {} (str/split tunnels #", "))]
      [name {:rate (parse-long rate) :tunnels neighbors}]))

  (defn all-connections [parsed-connections]
    (let [initial-connections (reduce (fn [acc k] (assoc-in acc [k :tunnels] {}))
                                      parsed-connections
                                      (keys parsed-connections))
          initial-lessons (into {} (mapcat (fn [[from {:keys [tunnels]}]]
                                             (map #(vector [from %] 1) (keys tunnels)))
                                           parsed-connections))]
      (loop [connections initial-connections, lessons initial-lessons]
        (if (seq lessons)
          (let [[[from to :as journey] cost] (first lessons)]
            (if (> (get-in connections [from :tunnels to] Integer/MAX_VALUE) cost)
              (let [new-lessons (reduce (fn [acc [nested-dest nested-cost]]
                                          (assoc acc [nested-dest to] (+ nested-cost cost)
                                                     [to nested-dest] (+ nested-cost cost)))
                                        {}
                                        (dissoc (get-in connections [from :tunnels]) to))]
                (recur (update-in connections [from :tunnels] assoc to cost)
                       (merge-with min (dissoc lessons journey) new-lessons)))
              (recur connections (dissoc lessons journey))))
          connections))))

  (defn restrict-paths-to-rooms [tunnel-map allowed-rooms]
    (reduce (fn [m k] (if (or (allowed-rooms k) (= "AA" k))
                        (update m k update :tunnels select-keys allowed-rooms)
                        (dissoc m k)))
            tunnel-map
            (keys tunnel-map)))

  (defn remove-broken-valve-rooms [tunnel-map]
    (->> tunnel-map
         (keep (fn [[name {:keys [rate]}]] (when (pos-int? rate) name)))
         set
         (restrict-paths-to-rooms tunnel-map)))

  (defn follow-tunnels-move [tunnel-map state]
    (let [{:keys [room open time-remaining]} state]
      (keep (fn [[room' dist]] (let [time-after-move-and-open (- time-remaining dist 1)]
                                 (when (and (not (open room'))
                                            (>= time-after-move-and-open 0))
                                   (let [released-in-transit (* (inc dist) (:rate state))]
                                     (-> (assoc state :room room' :time-remaining time-after-move-and-open)
                                         (update :open conj room')
                                         (update :released + released-in-transit)
                                         (update :rate + (get-in tunnel-map [room' :rate])))))))
          (get-in tunnel-map [room :tunnels]))))

  (defn give-up-move [state]
    (let [{:keys [rate time-remaining]} state]
      (-> (assoc state :time-remaining 0)
          (update :released + (* time-remaining rate)))))

  (defn next-steps [tunnel-map state]
    (let [options (follow-tunnels-move tunnel-map state)]
      (if (seq options)
        options
        [(give-up-move state)])))

  (defn max-pressure-released [time-limit tunnel-map]
    (loop [options [{:room "AA" :open #{} :rate 0 :released 0 :time-remaining time-limit}] most-pressure 0]
      (if-let [option (first options)]
        (if (zero? (:time-remaining option))
          (recur (rest options) (max most-pressure (:released option)))
          (recur (into (next-steps tunnel-map option) (rest options)) most-pressure))
        most-pressure)))

  ;; Part One
  (->> input
    (str/split-lines)
    (map parse-tunnel)
    (into {})
    (all-connections) 
    (remove-broken-valve-rooms)
    (max-pressure-released 30)))

(do
  (defn split-across-pairs [values]
    (reduce (fn [acc room] (concat (map #(update % 0 conj room) acc)
                                   (map #(update % 1 conj room) acc)))
            [[[(first values) "AA"] ["AA"]]]
            (rest values)))

  (defn tunnel-pairs [tunnel-map]
    (map (fn [pair] (map #(restrict-paths-to-rooms tunnel-map (set %)) pair))
         (split-across-pairs (keys tunnel-map))))

  (defn max-pressure-for-pair [pair]
    (apply + (map (partial max-pressure-released 26) pair)))

  (defn max-pressure-for-pair2 [pair]
    ;; pair)
    ;; (map #(first %) pair))
    ;; (map #(into [(first %) (max-pressure-released 26 %)]) pair))
    (map #(into [(keys %) (keys %) (max-pressure-released 26 %)]) pair))

  (def pair
    [{"JJ" {:rate 21, :tunnels {"II" 1}},
      "BB" {:rate 13, :tunnels {"CC" 1, "AA" 1}}
      "CC" {:rate 2, :tunnels {"DD" 1, "BB" 1}}}
     {"DD" {:rate 20, :tunnels {"CC" 1, "AA" 1, "EE" 1}}
      "HH" {:rate 22, :tunnels {"GG" 1}}
      "EE" {:rate 3, :tunnels {"FF" 1, "DD" 1}}}])

  (->> input
    (str/split-lines)
    (map parse-tunnel)
    (into {})
    (tunnel-pairs)
    (first)))
    ;; (map max-pressure-for-pair2)))
    ;; (reduce max)))
  

2490

;; Day 18

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

(def input "2,2,2
           2,2,1")

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

(defn adjacents [cubes]
  (for [dir [[-1 0 0] [1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
        cube cubes]
    (map + dir cube)))

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

;; Day 19

(def input "Blueprint 1: Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.
  Blueprint 2: Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian.")

(def input "")

(do
  (defn collect-&-build [{:keys [ore-robots clay-robots obsidian-robots geode-robots building-ore-robots building-clay-robots building-obsidian-robots building-geode-robots] :as state}]
    (-> state
      (update :ore + ore-robots)
      (update :clay + clay-robots)
      (update :obsidian + obsidian-robots)
      (update :geode + geode-robots)
      (update :ore-robots + building-ore-robots)
      (update :clay-robots + building-clay-robots)
      (update :obsidian-robots + building-obsidian-robots)
      (update :geode-robots + building-geode-robots)
      (assoc :building-ore-robots 0)
      (assoc :building-clay-robots 0)
      (assoc :building-obsidian-robots 0)
      (assoc :building-geode-robots 0)))

  (defn can-build-ore-robot? [{:keys [ore ore-robot-cost-ore]}]
    (>= ore ore-robot-cost-ore))

  (defn should-build-ore-robot? [{:keys [ore-robots ore-robots obsidian-robots obsidian-robot-cost-ore-robot-cost-obsidian] :as state}]
    (and
      (can-build-ore-robot? state)
      (< ore-robots 4)))

  (defn build-ore-robot [{:keys [id ore-robot-cost-ore time] :as state}]
    (println "id" id "building ore bot on time" time)
    (-> state
      (update :building-ore-robots inc)
      (update :ore #(- % ore-robot-cost-ore))))

  (defn can-build-clay-robot? [{:keys [ore clay-robot-cost-ore]}]
    (>= ore clay-robot-cost-ore))

  (defn should-build-clay-robot? [{:keys [time id clay obsidian ore-robots clay-robots obsidian-robots obsidian-robot-cost-clay geode-robot-cost-obsidian] :as state}]
    (let [ticks-until-enough-clay (quot (- obsidian-robot-cost-clay clay) (if (zero? clay-robots) 0.00001 clay-robots))
          ticks-until-enough-obisian (quot (- geode-robot-cost-obsidian obsidian) (if (zero? obsidian-robots) 0.00001 obsidian-robots))]
      (when (= id 2)
        (println "id" id "time" time "until-clay "ticks-until-enough-clay))
      (and
        (can-build-clay-robot? state)
        (< clay-robots 9))))
        ;; (> ticks-until-enough-clay ore-robots)
        ;; (> ticks-until-enough-obisian ore-robots))))

  (defn build-clay-robot [{:keys [clay-robot-cost-ore] :as state}]
    (-> state
      (update :building-clay-robots inc)
      (update :ore #(- % clay-robot-cost-ore))))

  (defn can-build-obsidian-robot? [{:keys [ore clay obsidian-robot-cost-ore obsidian-robot-cost-clay]}]
    (and 
      (>= ore obsidian-robot-cost-ore)
      (>= clay obsidian-robot-cost-clay)))

  (defn should-build-obsidian-robot? [{:keys [time obsidian ore-robots obsidian-robots geode-robot-cost-obsidian] :as state}]
    (let [ticks-until-enough-obisian (quot (- geode-robot-cost-obsidian obsidian) (if (zero? obsidian-robots) 0.00001 obsidian-robots))]
      (and
        (can-build-obsidian-robot? state)
        (> time 24))))
        ;; (> ticks-until-enough-obisian ore-robots))))
      

  (defn build-obsidian-robot [{:keys [obsidian-robot-cost-ore obsidian-robot-cost-clay] :as state}]
    (-> state
      (update :building-obsidian-robots inc)
      (update :ore - obsidian-robot-cost-ore)
      (update :clay - obsidian-robot-cost-clay)))

  (defn can-build-geode-robot? [{:keys [ore obsidian geode-robot-cost-ore geode-robot-cost-obsidian]}]
    (and 
      (>= ore geode-robot-cost-ore)
      (>= obsidian geode-robot-cost-obsidian)))

  (defn build-geode-robot [{:keys [geode-robot-cost-ore geode-robot-cost-obsidian] :as state}]
    (-> state
      (update :building-geode-robots inc)
      (update :ore - geode-robot-cost-ore)
      (update :obsidian - geode-robot-cost-obsidian)))

  (defn calculate-bottleneck [{:keys [id time ore-robots obsidian-robots obsidian-robot-cost-ore obsidian-robot-cost-clay geode-robot-cost-ore geode-robot-cost-obsidian ] :as state}]
    (let [desired-obsidian-ore-factor (quot geode-robot-cost-obsidian geode-robot-cost-ore)
          factual-obsidian-ore-factor (quot obsidian-robots ore-robots)
          desired-clay-ore-factor (quot obsidian-robot-cost-clay obsidian-robot-cost-ore)
          factual-clay-ore-factor (quot obsidian-robots ore-robots)
          bottlenecks (if (> factual-obsidian-ore-factor desired-obsidian-ore-factor)
                        #{:ore}
                        #{:obsidian})
          bottlenecks (if (> factual-clay-ore-factor desired-clay-ore-factor)
                        (conj bottlenecks :ore)
                        (conj bottlenecks :clay))]
      (when (and
              ;; (< time 3)
              (= id 2))
        (println "id" id "time" time "bottleneck" bottlenecks)
        ;; (println "desired-obsidian-ore-factor" desired-obsidian-ore-factor)
        ;; (println "factual-obsidian-ore-factor" factual-obsidian-ore-factor)
        ;; (println "desired-clay-ore-factor" desired-clay-ore-factor)
        ;; (println "factual-clay-ore-factor" factual-clay-ore-factor)
        (println))))

  (defn tick-robots
    [{:keys [ore-robots geode-robot-cost-ore obsidian-robot-cost-ore ore geode clay obsidian-robot-cost-clay geode-robots obsidian-robots ore-robot-cost-ore clay-robots clay-robot-cost-ore id obsidian geode-robot-cost-obsidian building-ore-robots building-clay-robots building-obsidian-robots building-geode-robots]
      :as state}]
    (let [bottleneck (calculate-bottleneck state)]
      (-> (dissoc state :action)
        (#(if-not (can-build-geode-robot? %) %
            (-> %
              (assoc :action :build-geode-robot)
              (build-geode-robot))))
        (#(if-not (should-build-obsidian-robot? %) %
            (-> %
              (assoc :action :build-obsidian-robot)
              (build-obsidian-robot))))
        (#(if-not (should-build-clay-robot? %) %
            (-> %
              (assoc :action :build-clay-robot)
              (build-clay-robot))))
        (#(if-not (should-build-ore-robot? %) %
            (-> %
              (assoc :action :build-ore-robot)
              (build-ore-robot))))
        (update :time inc)
        (collect-&-build))))

  ;; (defn max-geodes [state]
    ;;   (loop [time-left 2
    ;;          state state]
    ;;     (let [{:keys [ore-robots geode-robot-cost-ore obsidian-robot-cost-ore ore geode clay obsidian-robot-cost-clay geode-robots obsidian-robots ore-robot-cost-ore clay-robots clay-robot-cost-ore id obsidian geode-robot-cost-obsidian]} state]
    ;;       (cond 
    ;;         (<= time-left 0)
    ;;         state
    ;;       
    ;;         :else
    ;;         (recur
    ;;           (dec time-left)
    ;;           (-> state
    ;;             (update :ore #(+ % 10))
    ;;             (update :clay #(+ % 10))))))))

  (defn explore [{:keys [ore-robots geode-robot-cost-ore obsidian-robot-cost-ore ore geode clay obsidian-robot-cost-clay geode-robots obsidian-robots ore-robot-cost-ore clay-robots clay-robot-cost-ore id obsidian geode-robot-cost-obsidian]
                  :as state}]
    ;; (let [])
    ;; (println)
    ;; (println "--------" id "---------")
    ;; (println "--------------------")
    ;; (println)
    state)

  (->> input
    (clojure.string/split-lines)
    (map (fn [s]
           {:id (Integer/parseInt (second (re-find #"Blueprint (\d+):" s)))
            :ore-robot-cost-ore (Integer/parseInt (second (re-find #"Each ore robot costs (\d+) ore" s)))
            :clay-robot-cost-ore (Integer/parseInt (second (re-find #"Each clay robot costs (\d+) ore" s)))
            :obsidian-robot-cost-ore (Integer/parseInt (second (re-find #"Each obsidian robot costs (\d+) ore" s)))
            :obsidian-robot-cost-clay (Integer/parseInt (second (re-find #"Each obsidian robot costs \d+ ore and (\d+) clay" s)))
            :geode-robot-cost-ore (Integer/parseInt (second (re-find #"Each geode robot costs (\d+) ore" s)))
            :geode-robot-cost-obsidian (Integer/parseInt (second (re-find #"Each geode robot costs \d+ ore and (\d+) obsidian" s)))
            :building-ore-robots 0
            :building-clay-robots 0
            :building-obsidian-robots 0
            :building-geode-robots 0
            :ore-robots 1
            :clay-robots 0
            :obsidian-robots 0
            :geode-robots 0
            :ore 0
            :clay 0
            :obsidian 0
            :geode 0
            :time 0}))
    (map explore)
    (iterate #(map tick-robots %))

    (take 26)
    (map #(filter (comp (partial = 2) :id) %))
    (flatten)

    ;; (#(nth % 24))

    (map #(select-keys % [:time :ore-robots #_:id :ore :clay :obsidian :geode :clay-robots :obsidian-robots :geode-robots]))))
         ;; (re-seq #"Blueprint (\d+):\s*Each (\w+) robot costs (\w+|\d+)+" %))))

;; Day 20

(def input "1
  2
  -3
  3
  -2
  0
  4")

(def input "")

(require '[clojure.test :as t])
(mod -2 6)
(update-in {0 [1 2] 2 [3 4]} [2 1] inc)
(do
  (defn solve [input]
    (->> input
      (re-seq #"-?\d+")
      (mapv #(Long/parseLong %))
      (zipmap (range))

      ;; (map (fn [[k v]] {k v})))
      ;; (apply merge))

      ;; It's a map in the {k [p v]} format, p is a pointer,
      ;; in the start they are all pointing to the next.
      (#(into {} (for [[k v] %] [k [(mod (inc k) (count %))
                                    v]])))
      ((fn [original]
         (reduce
           (fn [mix to-mix]
             (let [[from n] (mix to-mix)
                   to (mod (+ from n) (count mix))]
               (loop [c (count mix)
                      k to-mix
                      mix mix]
                 (let [new-k (first (mix k))]
                   (if (<= c 1)
                     (update mix k assoc 0 to)
                     (recur
                       (dec c)
                       new-k
                       (update mix k assoc 0 (mod (inc new-k) (count mix)))))))))

           original (range 1 #_(count original)))))))

      ;; ((fn [original]
      ;;    (reduce
      ;;     (fn [mix to-mix]
      ;;       (let [[from n] (mix to-mix)
      ;;             to (+ from n)
      ;;             to (if (pos? to) to (mod to (dec (count mix))))
      ;;             to (if (< to (count mix)) to (mod to (count mix)))
      ;;             move-right #(if (<= from % to) (dec %) %)
      ;;             move-left #(if (<= to % from) (inc %) %)
      ;;             move (if (< to from) move-left move-right)
      ;;             new-mix (merge
      ;;                       (into {}
      ;;                             (for [[old-key [k v]] mix]
      ;;                               [old-key [(move k) v]]))
      ;;                       {to-mix [to n]})]
      ;;         ;; (println n (->> new-mix (map second) (flatten) (apply sorted-map) (map second)) "to" to "from" from)
      ;;         new-mix))
      ;;     original (range (count original)))))
      ;; (map second)
      ;; (flatten)
      ;; (apply sorted-map)
      ;; (map second)))
      ;; (reduce
      ;;   (fn [r [_ [k v]]]
      ;;     (if-not (#{1001 2001 3001} k)
      ;;       r
      ;;       (conj r v)))
      ;;   []))
      ;; (apply +))

  (defn solve [input]
    (->> input
      (re-seq #"-?\d+")
      (mapv #(Long/parseLong %))))

  (t/is (=        [0 1 0 0 0 0]
           (solve "1 0 0 0 0 0")))
  (t/is (=        [0 0 2 0 0 0]
           (solve "2 0 0 0 0 0")))
  (t/is (=        [0 0 0 3 0 0]
           (solve "3 0 0 0 0 0")))
  (t/is (=        [0 0 0 0 0 5]
           (solve "5 0 0 0 0 0")))
  (t/is (=        [0 6 0 0 0 0]
           (solve "6 0 0 0 0 0")))
  (t/is (=        [0 7 0 0 0 0]
           (solve "7 0 0 0 0 0")))
  (t/is (=        [0 0 8 0 0 0]
           (solve "8 0 0 0 0 0")))
  (t/is (=        [0 0 0 0 0 11]
           (solve "11 0 0 0 0 0")))
  (t/is (=        [12 0 0 0 0 0]
           (solve "12 0 0 0 0 0")))
  (t/is (=        [18 0 0 0 0 0]
           (solve "18 0 0 0 0 0")))
  (t/is (=        [0 0 0 0 -1 0]
           (solve "-1 0 0 0 0 0")))
  (t/is (=        [0 0 0 -2 0 0]
           (solve "-2 0 0 0 0 0")))
  (t/is (=        [0 -4 0 0 0 0]
           (solve "-4 0 0 0 0 0")))
  (t/is (=        [-5 0 0 0 0 0]
           (solve "-5 0 0 0 0 0")))
  (t/is (=         [0 0 0 0 -6 0]
           (solve "-6 0 0 0  0 0")))
  (t/is (=         [0 0 0 -7 0 0]
           (solve "-7 0 0 0  0 0")))
  ;; (t/is (=        [-18 0 0 0 0 0]
  ;;          (solve "-18 0 0 0 0 0")))
  (t/is (=        [-1 0 0 0 0 0]
           (solve "0 -1 0 0 0 0")))
  (t/is (=        [0 0 0 0 -2 0]
           (solve "0 -2 0 0 0 0")))
  (t/is (=        [0 0 0 -3 0 0]
           (solve "0 -3 0 0 0 0")))
  (t/is (=        [0 0 0 0 0 -2]
           (solve "0 0 -2 0 0 0")))
  (t/is (=        [0 0 0 0 -3 0]
           (solve "0 0 -3 0 0 0")))
  (t/is (=        [0  0 0 -3 0 0]
           (solve "0 -3 0  0 0 0")))
  (t/is (=        [12 -18 -5 24 30 36]
           (solve "12 -18 -5 24 30 36")))
  (t/is (=        [-8 12 -18 24 30 36]
           (solve "12 -18 -8 24 30 36"))))

(mod -3 6)
(-> input
  (clojure.string/split-lines)
  (count))

#{1000 2000 3000}
  ;; (flatten))
  ;; (distinct)
  ;; (count))
(mod 1997 7)

;; Day 21

(def input "root: pppw + sjmn
  dbpl: 5
  cczh: sllz + lgvd
  zczc: 2
  ptdq: humn - dvpt
  dvpt: 3
  lfqf: 4
  humn: 5
  ljgn: 2
  sjmn: drzm * dbpl
  sllz: 4
  pppw: cczh / lfqf
  lgvd: ljgn * ptdq
  drzm: hmdt - zczc
  hmdt: 32")

(def input "root: pppw + sjmn
  dbpl: 5
  cczh: sllz + lgvd
  zczc: 2
  ptdq: dvpt - humn
  dvpt: 3
  lfqf: 4
  humn: 5
  ljgn: 2
  sjmn: drzm * dbpl
  sllz: 4
  pppw: cczh / lfqf
  lgvd: ljgn * ptdq
  drzm: hmdt - zczc
  hmdt: 32")

(def input "root: pppw + sjmn
  dbpl: 5
  cczh: sllz + lgvd
  zczc: 2
  ptdq: dvpt - humn
  dvpt: 3
  lfqf: 4
  humn: 5
  ljgn: 2
  sjmn: drzm * dbpl
  sllz: 4
  pppw: cczh / lfqf
  lgvd: ljgn - ptdq
  drzm: hmdt - zczc
  hmdt: 32")

(def input-r "")
(def input input-r)

;; Part One
(do
  (defn math-monkey [monkey jobs-map]
    (let [yell-number? (number? (jobs-map monkey))
          [m1 op-str m2] (when-not yell-number? (jobs-map monkey))
          op ({"+" + "-" - "/" / "*" *} op-str)]
      (if yell-number?
        (jobs-map monkey)
        (op (math-monkey m1 jobs-map) (math-monkey m2 jobs-map)))))    

  (->> input
    (clojure.string/split-lines)
    (map #(re-find #"(\w+): (-?\d+|(\w+) ([-+/*]) (\w+))" %))
    (map #(into {(second %) (if (re-find #"\d+" (nth % 2))
                              (Long/parseLong (nth % 2))
                              [(nth % 3) (nth % 4) (nth % 5)])}))
    (into {})
    (math-monkey "root")))

(do
  ;; (def counter (atom 0))

  (def math-monkey-2 
    (memoize
      (fn [monkey jobs-map]
        (let [yell-number? (number? (jobs-map monkey))
              incognita? (= :? (jobs-map monkey))
              [m1 op-str m2] (when-not (or incognita? yell-number?)
                               (jobs-map monkey))
              op ({"+" + "-" - "/" / "*" *} op-str)
              result (cond
                       yell-number?
                       (jobs-map monkey)

                       incognita?
                       (str (jobs-map monkey))

                       (or (string? (math-monkey-2 m1 jobs-map)) (string? (math-monkey-2 m2 jobs-map)))
                       (str "("op-str" "(math-monkey-2 m1 jobs-map)" "(math-monkey-2 m2 jobs-map)")")

                       (and (number? (math-monkey-2 m1 jobs-map)) (number? (math-monkey-2 m2 jobs-map)))
                       (op (math-monkey-2 m1 jobs-map) (math-monkey-2 m2 jobs-map))    

                       :else
                       (do
                         (println "Unknown situation: m1:" m1 "op:" op "m2:" m2) 
                         (str "("op-str" "(math-monkey-2 m1 jobs-map)" "(math-monkey-2 m2 jobs-map)")")))]
          ;; (swap! counter inc)
          ;; (when (zero? (mod @counter 1))
          ;;   (println @counter result))
          result))))

  (defn solve-recur [result expr]
    (let [oritinal-op (first expr)
          op ({'* / '/ * '+ - '- +} oritinal-op)
          number (if (number? (second expr)) (second expr) (nth expr 2))
          expr-with-incognita (if (number? (second expr)) (nth expr 2) (second expr))
          result (if (and (= '- oritinal-op) (number? (second expr)))
                   (- result)
                   result)]
      ;; (swap! counter inc)
      ;; (when (zero? (mod @counter 1))
      ;;   (println @counter expr))
      (if (keyword? expr-with-incognita)
        (op result number)
        (solve-recur (op result number) expr-with-incognita))))
          
  (defn solve-incognita [expr]
    (assert (= '= (first expr)))
    (assert (or (number? (second expr)) (number? (nth expr 2))))
    (let [r (if (number? (second expr)) (second expr) (nth expr 2))
          expr-with-incognita (if (number? (second expr)) (nth expr 2) (second expr))]
      (solve-recur r expr-with-incognita)))

  (->> input
    (clojure.string/split-lines)
    (map #(re-find #"(\w+): (-?\d+|(\w+) ([-+/*]) (\w+))" %))
    (map #(into {(second %) (if (re-find #"\d+" (nth % 2))
                              (Long/parseLong (nth % 2))
                              [(nth % 3) (nth % 4) (nth % 5)])}))
    (into {})
    (#(merge % {"humn" :?}))
    (#(update % "root" assoc 1 "="))
    (math-monkey-2 "root")
    (edn/read-string)
    (solve-incognita)))

;; Day 22
;; guessed 80322
;; guessed 138264

(do
  (def manually-mapped-edges
    [[1 0 6 0 true]
     [1 2 3 3 false]
     [1 3 2 3 true]
     [2 1 5 1 true]
     [2 2 6 1 true]
     [2 3 1 3 true]
     [3 1 5 2 true]
     [3 3 1 2 false]
     [4 0 6 3 true]
     [5 1 2 1 true]
     [5 2 3 1 true]
     [6 0 1 0 true]
     [6 1 2 2 true]
     [6 3 4 0 true]])
    ;; I produced this data manually by looking at the input..
  (def input "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"))

(do 
  (def manually-mapped-edges
    [[1 3 6 2 false]
     [1 2 4 2 true]
     [2 3 6 1 false]
     [2 0 5 0 true]
     [2 1 3 0 false]
     [3 2 4 3 false]
     [3 0 2 1 false]
     [4 3 3 2 false]
     [4 2 1 2 true]
     [5 0 2 0 true]
     [5 1 6 0 false]
     [6 0 5 1 false]
     [6 1 2 3 false]
     [6 2 1 3 false]])
  (def input input-r))

(def input-r "")
(map-cube-edges state [[6 2 1 3 true]])

;; mapping edges
  ;;     11112222
  ;;     11112222
  ;;     11112222
  ;;     3333
  ;;     3333
  ;;     3333
  ;; 44445555
  ;; 44445555
  ;; 44445555
  ;; 6666
  ;; 6666
  ;; 6666
  ;;
(->> "1u 6l f
  1l 4l t ok
  2u 5r t ok
  2r 6d f ok
  2d 3r f ok
  3l 4u f ok
  3r 2d f ok
  4u 3l f ok
  4l 1l t ok
  5r 2u t ok
  5d 6r f ok
  6r 5d f ok
  6d 2r f ok
  6l 1u f"
  (clojure.string/split-lines)
  (map #(re-find #"\d[rlud] \d[rlud] [tf]" %))
  (map (fn [[q-out edge-out _ q-in edge-in _ reverse?]]
         (let [c->int #(Integer/parseInt (str %))
               edge->int {\r 0 \d 1 \l 2 \u 3}
               reverse? ({\t true \f false} reverse?)]
           [(c->int q-out) (edge->int edge-out) (c->int q-in) (edge->int edge-in) reverse?]))))
  ;;
  ;; From example
  ;;
(->> "1r 6r t
  1l 3u f
  1u 2u t
  2d 5d t
  2l 6d t
  2u 1u t
  3d 5l t
  3u 1l f
  4r 6u t
  5d 2d t
  5l 3d t
  6r 1r t
  6d 2l t
  6u 4r t"
  (clojure.string/split-lines)
  (map #(re-find #"\d[rlud] \d[rlud] [tf]" %))
  (map (fn [[q-out edge-out _ q-in edge-in _ reverse?]]
         (let [c->int #(Integer/parseInt (str %))
               edge->int {\r 0 \d 1 \l 2 \u 3}
               reverse? ({\t true \f false} reverse?)]
           [(c->int q-out) (edge->int edge-out) (c->int q-in) (edge->int edge-in) reverse?]))))

(do
  (defn move [{:keys [board current-dir current-pos] :as state}]
    (let [dir ({0 [1 0] 1 [0 1] 2 [-1 0] 3 [0 -1]} current-dir)
          [first-last x-y] ({0 [first second] 1 [first first] 2 [last second] 3 [last first]} current-dir)
          new-pos (map + dir current-pos)
          new-pos (if (nil? (board new-pos))
                    (first-last (sort (filter #(= (x-y current-pos) (x-y %)) (keys board))))
                    new-pos)
          new-pos (if (= \# (board new-pos))
                    current-pos
                    new-pos)]
      (assoc state :current-pos new-pos)))

  (def quadrant-edges 
    (memoize
      (fn [{:keys [board] :as state}]
        (let [face-size (int (Math/sqrt (/ (count (keys board)) 6)))
              max-x (apply max (map first (keys board)))
              max-y (apply max (map second (keys board)))
              x-quadrants (/ max-x face-size)
              y-quadrants (/ max-y face-size)]
          (for [yq (range 1 (inc y-quadrants))
                xq (range 1 (inc x-quadrants))
                :let [quadrant [(* xq face-size) (* yq face-size)]]
                :when (board quadrant)]
            (for [dir [[1 0] [0 1] [-1 0] [0 -1]]
                  y (range face-size)
                  x (range face-size)
                  :let [edge (mapv - quadrant [x y])
                        dir' ({[1 0] 0 [0 1] 1 [-1 0] 2 [0 -1] 3} dir)]
                  :when (nil? (board (map + edge dir)))]
              [dir' edge]))))))

  ;; Quadrant is from 1 to 6 and direction is 0 to 3
  (defn get-edge [state quadrant direction]
    (->> (dec quadrant)
      (nth (quadrant-edges state))
      (filter #(= (first %) direction))
      (map second)))

  (defn map-cube-edges [state mapped-edges]
    (into (sorted-map)
      (flatten
        (map (fn [[q-out edge-out q-in edge-in reverse?]]
               (map #(into {[%1 edge-out] [%2 (mod (+ 2 edge-in) 4)]})
                    (get-edge state q-out edge-out)
                    (cond-> (get-edge state q-in edge-in) reverse? reverse)))
             mapped-edges))))

  ;; (get-edge 1 2)
  ;; (map-cube-edges state [[1 0 6 0]])
  ;; ((:edges-mapping state) [[51 1] 3])
  ;; ((:board state) [1 151])
      
  (defn move-part-2 [{:keys [board current-dir current-pos edges-mapping] :as state}]
    (let [dir ({0 [1 0] 1 [0 1] 2 [-1 0] 3 [0 -1]} current-dir)
          new-pos (mapv + dir current-pos)
          [new-pos new-dir] (if (nil? (board new-pos))
                              (do
                                ;; (println current-pos current-dir new-pos)
                                (edges-mapping [current-pos current-dir]))
                              [new-pos current-dir])
          [new-pos new-dir] (if (= \# (board new-pos))
                              [current-pos current-dir]
                              [new-pos new-dir])]
      (-> state
        (assoc :current-pos new-pos)
        (assoc :current-dir new-dir))))
      
  (when state)
  ;;   (clojure.test/is (= [10 1] (:current-pos (move state))))
  ;;   (clojure.test/is (= [11 1] (:current-pos (move (assoc state :current-pos [11 1])))))
  ;;   (clojure.test/is (= [10 4] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [10 3])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   (clojure.test/is (= [9 4] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [12 4]))))))
  ;;   (clojure.test/is (= [12 3] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [12 3]))))))
  ;;   (clojure.test/is (= [1 6] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [12 6]))))))
  ;;   (clojure.test/is (= [9 9] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [16 9]))))))
  ;;   (clojure.test/is (= [9 1] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [9 12])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   (clojure.test/is (= [9 12] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [9 1])
  ;;                                                    (assoc :current-dir 3))))))
  ;;   (clojure.test/is (= [12 12] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [12 12])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   ;; move all directions
  ;;   (clojure.test/is (= [7 5] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [6 5])
  ;;                                                    (assoc :current-dir 0))))))
  ;;   (clojure.test/is (= [6 6] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [6 5])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   (clojure.test/is (= [5 5] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [6 5])
  ;;                                                    (assoc :current-dir 2))))))
  ;;   (clojure.test/is (= [6 5] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [6 6])
  ;;                                                    (assoc :current-dir 3))))))
  ;;   ;; wall in all directions
  ;;   (clojure.test/is (= [8 6] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [8 6])
  ;;                                                    (assoc :current-dir 0))))))
  ;;   (clojure.test/is (= [8 6] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [8 6])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   (clojure.test/is (= [9 7] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [9 7])
  ;;                                                    (assoc :current-dir 2))))))
  ;;   (clojure.test/is (= [9 7] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [9 7])
  ;;                                                    (assoc :current-dir 3))))))
  ;;   ;; teleport in all directions
  ;;   (clojure.test/is (= [9 12] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [16 12])
  ;;                                                    (assoc :current-dir 0))))))
  ;;   (clojure.test/is (= [16 9] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [16 12])
  ;;                                                    (assoc :current-dir 1))))))
  ;;   (clojure.test/is (= [16 12] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [9 12])
  ;;                                                    (assoc :current-dir 2))))))
  ;;   (clojure.test/is (= [16 12] (:current-pos (move (-> state
  ;;                                                    (assoc :current-pos [16 9])
  ;;                                                    (assoc :current-dir 3))))))
    ;; ;; ;; Part 2
    ;; ;; (clojure.test/is (= [[16 12] 2] ((juxt :current-pos :current-dir)
    ;; ;;                                  (move-part-2 (-> state
    ;; ;;                                                 (assoc :current-pos [12 1])
    ;; ;;                                                 (assoc :current-dir 0))))))
    ;; ;; (clojure.test/is (= [[15 9] 1] ((juxt :current-pos :current-dir)
    ;; ;;                                 (move-part-2 (-> state
    ;; ;;                                                (assoc :current-pos [12 6])
    ;; ;;                                                (assoc :current-dir 0))))))
    ;; ;; ;; Part 2 real input
    ;; ;; (clojure.test/is (= [[100 1] 2] ((juxt :current-pos :current-dir)
    ;; ;;                                  (move-part-2 (-> state
    ;; ;;                                                 (assoc :current-pos [101 1])
    ;; ;;                                                 (assoc :current-dir 2))))))
    ;; (is-part-2-move state [[1 150] 0] [[51 1] 2])
    ;; (is-part-2-move state [[1 152] 0] [[52 1] 3])
    ;; (is-part-2-move state [[1 200] 0] [[100 1] 3])
    ;; (is-part-2-move state [[2 200] 3] [[102 1] 3])
    ;; (is-part-2-move state [[49 200] 3] [[149 1] 3])
    ;; (is-part-2-move state [[100 150] 2] [[150 1] 0])
    ;; (is-part-2-move state [[1 101] 0] [[51 50] 2])
    ;; (is-part-2-move state [[100 51] 2] [[101 50] 1])
    ;; (is-part-2-move state [[100 101] 2] [[150 50] 0])
    ;; (is-part-2-move state [[100 100] 2] [[150 50] 1])
    ;; (is-part-2-move state [[1 101] 1] [[51 51] 2])
    ;; (is-part-2-move state [[101 50] 3] [[100 51] 0])
    ;; (is-part-2-move state [[50 101] 1] [[51 100] 2])
    ;; (is-part-2-move state [[150 50] 3] [[100 100] 0])
    ;; (is-part-2-move state [[51 51] 0] [[1 101] 3])
    ;; (is-part-2-move state [[51 50] 0] [[1 101] 2])
    ;; (is-part-2-move state [[51 100] 0] [[50 101] 3])
    ;; (is-part-2-move state [[150 50] 2] [[100 101] 0])
    ;; (is-part-2-move state [[51 1] 0] [[1 150] 2])
    ;; (is-part-2-move state [[50 151] 2] [[51 150] 1])
    ;; (is-part-2-move state [[150 1] 2] [[100 150] 0])
    ;; (is-part-2-move state [[50 199] 2] [[99 150] 1])
    ;; (is-part-2-move state [[51 1] 1] [[1 151] 2])
    ;; (is-part-2-move state [[51 150] 3] [[50 151] 0])
    ;; (is-part-2-move state [[101 1] 1] [[1 200] 1])
    ;; (is-part-2-move state [[100 1] 1] [[1 200] 2])
    ;; (is-part-2-move state [[100 150] 3] [[50 200] 0])
    ;; (is-part-2-move state [[150 1] 1] [[50 200] 1]))

  (defn is-part-2-move [state expected [cur-pos cur-dir]]
    (clojure.test/is
      (= expected ((juxt :current-pos :current-dir)
                   (move-part-2 (-> state
                                  (assoc :current-pos cur-pos)
                                  (assoc :current-dir cur-dir)))))))

  (->> input
    (clojure.string/split-lines)
    (split-with #(re-find #"[^\d]" %))
    ((fn [[board [_ moves]]]
       {:moves (re-seq #"\d+|[RL]" moves)
        :board (into {}
                     (flatten
                       (map-indexed (fn [row line]
                                      (map-indexed (fn [column c]
                                                     (if (= \space c)
                                                       {}
                                                       {[(inc column) (inc row)] c}))
                                                   line))
                                    board)))}))
    (#(assoc % :current-dir 0))
    (#(assoc % :current-pos (first (sort (filter (fn [[_ y]] (= 1 y)) (keys (:board %)))))))
    (#(assoc % :edges-mapping (map-cube-edges % manually-mapped-edges)))
    ;; (def state)))

    ;; Part 1
    (#(reduce (fn [state action]
                (if-let [turn ({"R" inc "L" dec} action)]
                  (update state :current-dir (fn [old] (mod (turn old) 4)))
                  (nth (iterate move-part-2 state) (Long/parseLong action))))
              % (:moves %)))
    (#(dissoc % :board :moves :edges-mapping))
    ((fn [{dir :current-dir [column row] :current-pos}]
       (+ (* 1000 row) (* 4 column) dir)))))

    ;; (quadrant-edges)))

;; Day 23

(def input "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(def input ".....
..##.
..#..
.....
..##.
.....")

(def input real-input)

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
        (update :counter inc)
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

;; Day 24

(def input "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(def input "#.####################################################################################################
#<<v<.^<<<v.^v<>v^^vvv<<v<^><<>^^^v^v<^<v^>>.<v<v>^vv>>vvv<^<v>v^.<v^>>>>v<^^<v^v<v>>v^.v><<^vv^<><>>#
#<<v^.<^><.<<v<vv<vv><.<^.><>vv>v<>v^^>vv^<<.<^<>.v>vv>v^v>vv<v>.<<^^<.^...<<v^.^.v>>.<<><^v>vv>>v^^<#
#>v<vv>^<^>^^^^^>v<v>v.v.v>v^v^<.^^<^>^<>>^v>v<^.v^^v<^>^>^>vvv<>>.^<>^>^>v>.>.<>v..v>>vvv^>>>vv<^vv>#
#<<>v^.>v^>>v<^><v>.^v.<^v^<<<>v><>^><>v<.v>^v^><v^<>>vvv^>>>v.v.<^^.^<v<^.<<v^<<<vv<v<<>>v><v>^<^<v>#
#>>vv<v>^^^<vv<>^>>>^.<v^<^^>.v>^<>v<<..v<>^v<v>>vvv..v>v<>>^>>^>>^^^<.^^<^>v<.v<^^v^.>^<v><>.^<^^>>.#
#<v<><.<><<<^v<v<<v<>.<^>^v>v^.^.<v>vv>v..^><^.<v<<>v<^>>v<>vvv<vv<<><vv<>>^><.v<v<^^v^>v>>v>v^<vv^^<#
#>^^v^><<<^.^<v><><v^^^^.^>>>>.^v.v<.<^v>v><>^.>>.^>v>>^>>^>^><^^^<v^^^>v<^>^^<vvv<^v^<>.^vv<^>.^>^^<#
#<<vv^^^><<<v.<v>^>v>^^><v<<><<^<<<v>.<v^<<>><>^>>>><v<.^>>>><^<>>v^vv^<.<>.>>.^v>.v^>.>vvvv^v<><v^<<#
#>>v>>^<<v.^v.vvvv<>>><>^v<^<>><v<^vv<.v>v>^<v<>>^<>>>^.>^v<>>vv<vvv^<vv.<<.v<<>.>^^<>^<>^v^^^>>>^.^>#
#>><>vv>^>>v^>v>^^v.<<^v.<..>>>^^vvvv..^v>^>.<.v<^<<.><.^>.<v><<>.<v^vv><^<<>v^vv^>v<^.^^>^<<^.<<<>.<#
#<>v>>v<>>>.v<<v<^^v<>^v<^^<v.>^<^v>vv>^>v<vv><v<v<^>>vv>>>v.<<<><.<><>.<>>^><<^vvvvv^<<<v<vv>>.<<^<.#
#><<.>^v<>^.vv^>^><><.<^>>vv^vvv><v><.>^^^v^..><><.vv<^>v.v<vvv><^^v.<>v<.^v<^>>>v<v^<>^>v<v^v>^v><>>#
#><v>.>^<<v>>.<<<<vv^<<^v^>vv<^^v><>><<v<^>vv>^^>v^^^<<v.vv<><^>>>>>>>^>^^.>>^.<>^<^<v.^.<v^^>.>>v^<<#
#>><<.<^<.<<>vv<>>v<.<><^^>^<>v^<<^^^>^^<<^<>^<<v<^^^.v>v<^v>.<v<.vv>^>><.>^<.>.>>v^><<v<^<^.><.^.^v<#
#>v^^v>v><^><<^^^<<vv>>.v>^<>^^>v>^.v>>v^<^v<v^^>vv^v><>^>><^<v.v>v><<<<>v.<.^>^<v.<<<<v^<<<<>>^>v^<>#
#<^^v<<^>v.<<>^vv^^<^<v>>vvv^^<>>^v^v..v^v>v.^<^.<<v<>v><.<^<^v<>^>v<vv>v><<.vv^.>^>v..>><<.v<^v^>>^.#
#>v^v.v^v><>vv<^v^<v^v<>v<v<<>>..^.<vv^v>v.>^<vv>><^>v>>^^v^vv<^<^>>v^v>^^^><v.>>^^.v.^>><<><.vv.^>>>#
#.^>>.<^v.>vvv<v>>^<.v<v<<v^v>v.>.>v<^.<^.v.<.>vvv^vv><.^^v>>>v>^>^vv<<.<^<>>vv.^>vvvvv^><><v^<^v^>v.#
#<>v^^^.>^>>>^>v.<^.<<^^<>>v>vv.v.^.><v^<v^v<^>^>><vv>^.^.<^^<<<<v>^.>.<<.>.>^><v.v^.^<><^>^v><.vv^<<#
#>>vv^^<v<<v<.>v>^...>vv..^v^>^<>v^^^<<<.>^v>>>>.>^<.v><v><^^^vv>.v^>v.>v^>>><^v<>vv>v>v.^><^v<^^<vv>#
#>^<><<.>.>>v>>v<<^^v^.v^>.>>vv^.><<v^<v^>>>vvv<v<.<>^>>>>v>^^^.v^<.v><<v><.<v^<<<^>v^v^^^.>>>^>>v<><#
#<^v<^<<^<v^>^^<.<..vvv^^v^^><<.>^v<<vv<<>v<>><<>>v^.<v><>.<^vv<<>>>>^><v<<^v^vv.v.<vv^>^>><<^<<<v^^>#
#<v>^><><^^v>><<^>>^>><.>>v<^v>^v.<<v^>^<v>vv^^<<v><vvv>^.<v^v<^v^.^<<^<v>v^v>v.>v<>><v.v.><<^vvv^>.<#
#>><.<><>>>v...v<><v>v<^>><vv^<^^<v<v^<v>^.<^v><>>v<v^v>v^>>v<^>v<>.vv<<^^^v<>v<<^><^<v^.<^<^.>v^..><#
#><<>^vv.v<<v<<^v.><>^.<.<<<^^.v^<v^<vvv^v^>^>><^<>^>>^>^vv.^>vvvv<^<v.>^<.^vvv<><.^^>>^..v<^^<><<>><#
#<.><v<><^.><<.^<^^v^v><>^>.<^>.<v^^^>><^vv^vv<<>v.vv<^.^>^>.>>.<<^v>>^^.vv>vvv<vv>^>>>v^<>v.>.>v<>>.#
#<v>>>>^><^v^>^^vvv^<<v<v<<^^><^<vv><v>>v<>v^^>v>.<>v>^>v>>v.>.>>>^^^vv<<<>^><^<><v<^^^^^.<^vvv^<<<.<#
#.<v^<>>vv<^v^<v>>v<><.>.^^>^<v^>>v<<.<>v>^^v^v<>vv^^v>v>vvvvv>v^^<<^<><^v<v>>^<<v>>^.^v<^<<<<^v<.<><#
#<.^v>^vv^>^v<>>^^v.v<vvv.v<<^.>v><^vv>><.^<><v>..><>>.vv>>>.>v<v><v^..<v<<<^^><v<v<.v^..<v.^v<>>><^>#
#>vv.<..>^vv<^><>v>^vv<^.>^.<v^<v>vvv>>><<^v<>^<^v>^v.^v^v^><^><^<v.^vv<>^v>^<<^v^^>^^>^.v<>.<>v>>.^>#
#<v^^v<.<<vv.<>v^<^>^.<^<v<v^<<v<v.<^^>^>^v>>.<v.>^v.^v.^><<^>v<v.^vv^.^^>>>^<<>>^^<vv>v.<>^.v>^v.>v>#
#<>v>>^^<<>^^>><^vvv^^<<><><vv<vv>v>>>^><^.<v<^.>>v<<<vv.^<<^^><v><^.<><<v>^v^<^><>v..><<...<^.v.^v.>#
#>.>^>v><..^<^^^v^v<v><<^.^>.^v^v<^>>v><<.>v^<<^<<>^<.>>>v<.>>v>^^.<<>>^><v..v<vvvvv^v.<<<v..^^>.<^<<#
#<v>>.^<..>><>v^^>^^>.^^v^vv<>.<<v>><^<^<.^>^^>^<<.v^<<v^.^>v^^>>>.v><>^^<<><^<vv>^v.<^<v<<vv^v.<<<v<#
#.^^^<<v>v^vv^vv^>v^<v<^.v<^^>v^.^^^^>^<<^^>.>^^>^v<.vv>><v^v<vvv^>^>^<^^<v<>.v>^<v.<v^>.<v^.^>v^vv<>#
####################################################################################################.#")

(count input)

(do
  (defn move-blizzards [{:keys [blizzards max-x max-y] :as state}]
    (let [move #(mod (+ %1 %2) %3)]
      (update state :blizzards
              #(map (fn [[b d]]
                      [(mapv move b d [max-x max-y]) d])
                    %))))

  (def break (atom 0))
  (defn possible-moves [{:keys [blizzards max-x max-y current target]}]
    (let [all-possibilities [[0 1] [1 0] [0 0] [-1 0] [0 -1]]
          blizzards-set (set (map first blizzards))
          possible? (complement
                      (fn [[x y :as pos]]
                        (and (not= pos [0 -1])
                             (not= pos target)
                             (or (blizzards-set pos)
                                 (when x (< x 0))
                                 (when y (< y 0))
                                 (when x (>= x max-x))
                                 (when y (>= y max-y))))))]
      (filter #(possible? (mapv + current %)) all-possibilities)))

  (->> input
    (clojure.string/split-lines)
    ((fn [lines]
       {:current [0 -1]
        :target [(dec (clojure.string/index-of (last lines) \.)) (- (count lines) 2)] 
        :max-x (- (count (first lines)) 2)
        :max-y (- (count lines) 2)
        :steps []
        :blizzards (->> (map-indexed (fn [row line]
                                       (map-indexed (fn [column c]
                                                      (when-let [dir ({\> [1 0] \v [0 1] \< [-1 0] \^ [0 -1]} c)]
                                                        [[(dec column) (dec row)] dir]))
                                                    line))
                                     lines)
                     (apply concat)
                     (remove nil?))}))
    ((fn [height-to-search-increment {:keys [target] :as state}]
       (loop [to-test [state]
              visited #{}]
         (swap! break inc)
         (let [{:keys [current steps] :as testing-state} (first to-test)
               moved-blizzards (move-blizzards testing-state)
               possible-dirs (possible-moves moved-blizzards)
               move (fn [dir]
                      (-> moved-blizzards
                        (update :current #(map + % dir))
                        (update :steps conj dir)))
               new-visited (conj visited [current (count steps)])
               next-states (map move possible-dirs)]
               ;; next-states (remove #(visited [(:current %) (count (:steps %))]) (map move possible-dirs))]
           ;; (when (> @break 1000)
           ;;   (reset! break 0)
           ;;   (print "."))
           ;; (println (map #(count (:steps %)) to-test))
           ;; (println visited)
           (println possible-dirs)
           (println (map :current new-to-test))
           (println current (count steps))
           (println) 
           (if (or (= current target) (> @break 80))
             testing-state
             (recur
               (into (vec (rest to-test)) next-states)
               new-visited))))) 4500)
    (:steps)
    (count)))

    ;; (iterate move-blizzards)
    ;; (#(nth % 1)) 
    ;; (possible-moves)))
(into (rest [1 2 3]) '(4 5 6))
(let [[a & r] (into '(1 2) [3 4])]
  [a r])
(into nil (into [2 3] [4 5]))

;; Day 25

(def input "
  Decimal          SNAFU
        1              1
        2              2
        3             1=
        4             1-
        5             10
        6             11
        7             12
        8             2=
        9             2-
       10             20
       11             21
       12             22
       13            1==
       14            1=-
       15            1=0
       16            1=1
       17            1=2
       18            1-=
       19            1--
       20            1-0
       21            1-1
       22            1-2
       23            10=
       24            10-
       25            100
       26            101
       27            102
       28            11=
       29            11-
       30            110
     2022         1=11-2
    12345        1-0---0)
314159265  1121-1110-1=0
  ")

(def input "1=0=01=0=2111-1211
1==21=
20--
1-=1200=-=0
211==12=2=0111==
2=-10
1221=221=0-1=-
12=02=-12020=12=01-
12112101
20
120120=21=1002-1
10==1-02=1
1=11200001-=--
1=222-1=012=2
20===1-1102-=
1020
102=-==-=2-
112-2112-00012-
1010=--121
202022=-=2-22=
122==1=1020011=-00
10212==-0
2=--12020=1-0=-2=-
1110-1-2
20==210221=-21--12
1=-02--=22=-122
2=-=0=00=1--1-2=--
2==2==2-
2=12=1
2=01=
202=20202-000
1=0-=020021022012
2=0-011-=2=-0-=21
1=211
1-=1=11=--010=---
1000020-12==
2-=122-1211-0=
10=2220122012
2=21-01=212
10111==2=2-12
111
1=2---2211=2011=--
1--=0000=1-020
2=02-=-
1-011=
12020-0=-1-220100
2-==000=21=1
2-0100000-12-2-=
1-=1
1221=2-1==02-=202
200
122-2-0
10=2101-22
1==-120-1
2=2=2=-11--==-==
2111-=2=-=-2
1--
1-0=10-0112-12=
10=
2=01=1=-=20=
1=
1211-2=-1=
222=
122=2-1-0
1-2=20-=02122-1
1==-=2===0-=1100--11
12-0=201=10=-=
1=01=2=22-2-1-1
120==222100=0
11=-=--12=2-1101
102=1221=---0==021
2-=-2=202-=-==
100=1=02020-221=
1=0=-220=
1001-
20==2==1-1
1=2-2=2=-12=
10=0--2
10-202-2002=22=
2=2=
1-011=0=-12---
1002220
2=
1--=2---111
222==2
11100
1=0=201
1--02-
1=11-==0=021
1-=00=
1==12-20-=002001=
1=0=-=21-
1000-1=0-0=1=01-2-
2==
1-0
1=101-0-1
1=0=
11=2==-0-2202-0=
10-0=0=1-002
1-=0210=01120-
1112=2-0-2=-01
1-1=022=21
2=1-2--120
10002--0=220-0-2=12
11002102100
11000-2-12
120=-==1-2-=
21
11
2=121-
1==
1=-
1-=--2=-
2=-10-=0
2200-1-
2=21-1200==
1-102210=002=--112
1-10
122=202=201-=-10-1
2212121=02102
21=2-211=-1-20-
101-=01-1
200012112=
22=21201=22
1-1212=1-=1
1212-==1=01
22==--=
1=-2-1-==020-10-=
1=1210=--00
1=--=1=
2010111-00111-2=2
11=21-0=2-01
21=
1=-0-220
102-=0
1=---0-
1-1122-=121222--0
10110=1-20--1-1
20=--21220-=
1=---22-
2-000===1101=
20-
10==")

(Math/pow 5 1)

(do
  (defn snafu->decimal [snafu]
    (second
      (reduce (fn [[idx acc] digit]
                (let [pow (long (Math/pow 5 idx))
                      mult ({\2 2 \1 1 \0 0 \= -2 \- -1} digit)]
                  [(inc idx) (+ acc (* pow mult))]))
              [0 0]
              (reverse snafu))))

  (snafu->decimal "1-=")

  (defn decimal->snafu [decimal]
    (let [reminders (loop [to-div decimal
                           to-div-ahead (+ decimal 2)
                           reminders []]
                      (let [snafu-map {0 "0" 1 "1" 2 "2" 3 "=" 4 "-"}]
                        (if (<= to-div 0)
                          reminders
                          (recur
                            (quot (+ 2 to-div) 5)
                            (quot (+ 2 to-div-ahead) 5)
                            (conj reminders (snafu-map (rem to-div 5)))))))]
      (apply str (reverse reminders))))
      ;; (apply str
        ;; (map #(into [% ({[4 3] "1-", [2 2] "2", [0 0] "0", [1 0] "1", [3 3] "1=", [1 1] "1", [4 2] "2", [1 4] "1-", [0 3] "1=", [2 0] "0", [0 4] "10", [3 1] "1", [2 1] "2=", [4 4] "1=", [3 2] "1="} %)])
        ;;      (reverse reminders))))

  ;; (def digit-map {[0 1] "1" [0 2] "2" [0 3] "1=" [1 3] "2="})
  (map #(into [% (decimal->snafu %)]) (range 30))
  (decimal->snafu 4890)

  ;; (map #(into [% (map (fn [r] [r (digit-map r)]) (decimal->snafu %))]) (range 30))

  (->> input
    (clojure.string/split-lines)
    (map snafu->decimal)
    (apply +)
    (decimal->snafu)))
;;   Decimal          SNAFU
;;         1              1
;;         2              2
;;         3             1=
;;         4             1-
;;         5             10
;;         6             11
;;         7             12
;;         8             2=
;;         9             2-
;;        10             20
;;        15            1=0
;;        20            1-0
;;      2022         1=11-2
;;     12345        1-0---0)
;; 314159265  1121-1110-1=0
