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

(do ;; Part One
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

(do ;; Part Two
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
