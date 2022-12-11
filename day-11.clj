;; Couldn't finish part 2 yet 😔
(def input-example
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def real-input
  "Monkey 0:
  Starting items: 98, 97, 98, 55, 56, 72
  Operation: new = old * 13
  Test: divisible by 11
    If true: throw to monkey 4
    If false: throw to monkey 7

Monkey 1:
  Starting items: 73, 99, 55, 54, 88, 50, 55
  Operation: new = old + 4
  Test: divisible by 17
    If true: throw to monkey 2
    If false: throw to monkey 6

Monkey 2:
  Starting items: 67, 98
  Operation: new = old * 11
  Test: divisible by 5
    If true: throw to monkey 6
    If false: throw to monkey 5

Monkey 3:
  Starting items: 82, 91, 92, 53, 99
  Operation: new = old + 8
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 2

Monkey 4:
  Starting items: 52, 62, 94, 96, 52, 87, 53, 60
  Operation: new = old * old
  Test: divisible by 19
    If true: throw to monkey 3
    If false: throw to monkey 1

Monkey 5:
  Starting items: 94, 80, 84, 79
  Operation: new = old + 5
  Test: divisible by 2
    If true: throw to monkey 7
    If false: throw to monkey 0

Monkey 6:
  Starting items: 89
  Operation: new = old + 1
  Test: divisible by 3
    If true: throw to monkey 0
    If false: throw to monkey 5

Monkey 7:
  Starting items: 70, 59, 63
  Operation: new = old + 3
  Test: divisible by 7
    If true: throw to monkey 4
    If false: throw to monkey 3")

(def input input-example)

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
