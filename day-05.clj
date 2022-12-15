(def input
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(require '[clojure.string :as string])
(def stacks
  (->> input
    string/split-lines
    (map #(re-seq #"\s{4}|\[[A-Z]\]" %))
    (take-while some?)
    (map #(map (fn [[a b & r]]
                 (if (= \space a)
                   ""
                   (str b))) %))
    (reverse)
    (apply mapv vector)
    (map #(remove string/blank? %))
    (mapv #(into [] %))))

(def moves
  (->> input
    string/split-lines
    (drop-while (fn [[a & _r]] (not= a \m)))
    (map #(re-find #"move (\d+) from (\d+) to (\d+)" %))
    (map #(drop 1 %))
    (map (fn [a] (mapv #(Integer/parseInt %) a)))))

;; Part One
(defn move-stack [stacks from to]
  (let [stack-from (nth stacks (dec from))
        el (last stack-from)
        stack-to* (conj (nth stacks (dec to)) el)
        stack-from* (into [] (butlast stack-from))]
    (-> stacks
      (assoc (dec from) stack-from*)
      (assoc (dec to) stack-to*))))

(defn n-move-stack [stacks [n from to]]
  (nth (iterate #(move-stack % from to) stacks) n))

(apply str (map last (reduce n-move-stack stacks moves)))
  
;; Part Two
(defn move-stack-9001 [stacks [n from to]]
  (let [stack-from (nth stacks (dec from))
        [stack-from* els] (split-at (- (count stack-from) n) stack-from)
        stack-to (nth stacks (dec to))
        stack-to* (into [] (apply conj stack-to els))]
    (-> stacks
      (assoc (dec from) (into [] stack-from*))
      (assoc (dec to) stack-to*))))

(apply str (map last (reduce move-stack-9001 stacks moves)))
