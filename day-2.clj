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

