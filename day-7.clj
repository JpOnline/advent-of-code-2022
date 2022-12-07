;; Couldn't finish quick because I had a bug where I was using only the dir
;; name instead of the dir path to accumulate its size.
(require '[clojure.string :as string])

(def input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn process-commands [input]
  (reduce
    (fn [[dirs-size level r] line]
      (let [[cd? to] (re-find #"\$ cd (.*)" line)
            [dir? dir-name] (re-find #"dir (.*)" line)
            k-dir-name (keyword dir-name)
            [file? raw-file-size file-name] (re-find #"(\d+) (.*)" line)
            file-size (when file? (Long/parseLong raw-file-size))
            drop-last #(take (- (count %2) %1) %2)
            accumulate-dir-size #(update %1 (string/join "/" (drop-last %2 level)) + file-size)]
        (cond
          (and cd? (= to ".."))
          [dirs-size
           (vec (butlast level))
           r]

          cd?
          [dirs-size
           (conj level (keyword to))
           r]

          dir?
          [(assoc dirs-size (string/join "/" (conj level k-dir-name)) 0)
           level
           (assoc-in r (conj level (keyword dir-name)) {:files []})]

          file?
          [(reduce accumulate-dir-size  dirs-size (range (count level)))
           level
           (update-in r (conj level :files) conj [file-size file-name])]

          :else
          [dirs-size level r])))
    [{":/" 0} [] {:/ {:files []}}]
    (string/split-lines input)))

;; Part One
(->> input
  (process-commands)
  (first)
  (remove #(>= (second %) 100000))
  (map second)
  (apply +))     

;; Part Two
(->> input
  (process-commands)
  (first)
  (map second)
  (sort)
  (filter #(>= (+ % (- 70000000 (get a ":/"))) 30000000))
  (first))
