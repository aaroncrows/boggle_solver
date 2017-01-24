(ns clojure-word-game.core
  (:gen-class))

(require '[clojure.string :as s])

(declare str-to-keys)

(defn generate-word-set
  []
  (filter
    #(= % (s/lower-case %))
    (s/split (slurp "/usr/share/dict/words")
    #"\n")))
(def w (generate-word-set))

(defn str-to-keys
  "Turns a string into a seq of keywords"
  [string]
  (map #(-> % str keyword) string))

(defn random-letter
  "Returns a random capital letter from A-Z"
  [& args]
  (-> 26 rand-int (+ 65) char str))

(defn random-letter-vec
  "Returns a vector of random letters"
  [length]
  (mapv random-letter (range length)))

(defn make-trie
  [words]
  (loop [[curr & rest] words trie {}]
    (if (nil? curr)
     trie
     (recur rest (update-in trie
      (str-to-keys curr)
      assoc :word curr)))))

(def trie (make-trie (generate-word-set)))

(defn make-board
  "makes a game board of random letters"
  [s]
  (vec (for [x (range s)]
    (random-letter-vec s))))

(def board (make-board 3))

(defn coords-and-adjacent
  [[y x] s]
  (for [mod-y [-1 0 1] mod-x [-1 0 1]
          :when (and (not (every? #(= 0 %) [mod-x mod-y]))
                     (<= 0 (+ y mod-y) (- s 1))
                     (<= 0 (+ x mod-x) (- s 1)))]
    [(+ mod-y y)(+ mod-x x)]))
(defn grid-coords
  [grid]
  (for [[y row] (map-indexed vector grid)
        [x val] (map-indexed vector row)]
  [y x]))

(defn in-vec?
  [sub vec]
  (some #(= sub %) vec))

(defn each-adjacent
  [grid coords]

  (loop [[adjacent & remaining-adjacent] (coords-and-adjacent coords) visited [coords] string (str (get-in grid coords) "") word-matches []]
    (if (not adjacent)
      word-matches
      (if (in-vec? adjacent visited)
       (recur remaining-adjacent visited string word-matches)
        (do (when (get-in grid adjacent) (print remaining-adjacent "fuuuu"))

        (recur (concat remaining-adjacent (filter #(in-vec? % visited)(coords-and-adjacent adjacent))) (conj visited adjacent) (str string (get-in grid adjacent)) (conj word-matches string)))
      ))))

(defn is-prefix?
  "determines if string is a valid prefix"
  [trie string]
  (->> string str-to-keys (get-in trie) boolean))

(defn visit
  [coords visited string grid]
  [(into visited [coords]) (str string (get-in grid coords))])

(defn traverse-path-map
  [{nxt :next vst :visited s :string} current grid]

)

(defn all-paths
  [grid coords]
  (def grid-size (count grid))

  (loop [[path-map & remaining] [{:current coords :string "" :visited #{}}]
         word-matches []
         l 0]

      (if (or (not path-map) (> l 1000))
        word-matches
        (let [new-maps
          (for [coords (coords-and-adjacent (:current path-map) grid-size)
                :let [{s :string v :visited c :current} path-map]
                :when (not (contains? v coords))]
                (do {:current coords :string (str s (get-in grid c)) :visited (into v [c])}))
              mapss (if (not remaining) new-maps (into remaining new-maps))
              word (:word (get-in trie (str-to-keys (:string path-map))))
              ]
        (do
            #_(print (:word (get-in trie (str-to-keys (:string path-map)))))
           (recur  mapss (if word (conj word-matches word) word-matches) (+ l 1)))
  )))
    #_(if (or (not vertex) (> l 200))
      word-matches
      (let [[visited string] (visit vertex visited string grid)]
      (recur
        (remove visited (into neighbors (coords-and-adjacent vertex grid-size)))
        visited
        string
        (conj word-matches string)
        (inc l))))
)
;broken inner loop and outer loop run one then the other i have no idea what im doing
#_(defn list-matches
  [grid]
  (def grid-size (count grid))
  (loop [[coords & remaining-coords] (grid-coords grid) match-words []]
    (print "OUTER")
    (if (not coords)
    match-words
  (recur remaining-coords
    (conj match-words (loop [[vertex & neighbors] (coords-and-adjacent coords grid-size)
                                visited #{vertex}  string (get-in grid coords) l 0]
    (print "INNER")
    (if (or (not vertex) (< l 200))
      string
      (recur (remove visited (into neighbors (coords-and-adjacent vertex grid-size))) (into visited [vertex]) (str string (get-in grid vertex)) (inc l)))
    #_(print visited adjacent "pip")
    #_(if (or (not (get-in grid adjacent)) (in-vec? adjacent visited))
      (conj match-words string)
      (recur (concat remaining-adjacent (coords-and-adjacent adjacent))(conj visited adjacent) (str string (get-in grid adjacent)))
    ))
)))))
(def test-grid [["c" "a" "t"]
                ["b" "t" "o"]
                ["d" "g" "r"]])
(set (filter #(and (< 2 (count %)) (is-prefix? trie %))(flatten (for [square (grid-coords test-grid)]
(all-paths test-grid square)))))

