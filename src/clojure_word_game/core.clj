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

(defn adjacent-coords
  [[y x]]
  (for [mod-y [-1 0 1] mod-x [-1 0 1]
          :when (not-every? #(= 0 %) [mod-x mod-y])]
          [(+ mod-y y)(+ mod-x x)]))
(defn grid-coords
  [grid]
  (for [[y row] (map-indexed vector grid)
        [x val] (map-indexed vector row)]
  [y x]))

(defn each-adjacent
  [f grid coords]
  (def adjacent-coords (adjacent-coords coords))

  (loop [[adjacent & remaining-adjacent] adjacent-coords]
    (if adjacent
      (do (f (get-in grid adjacent))
        (recur remaining-adjacent)))))

(defn is-prefix?
  "determines if string is a valid prefix"
  [trie string]
  (->> string str-to-keys (get-in trie) boolean))
(defn in-vec?
  [sub vec]
  (some #(= sub %) vec))

(defn list-matches
  [grid coords]
  (loop [match-words []]
  (loop [[adjacent & remaining-adjacent] (adjacent-coords coords) visited [] string (get-in grid coords)]
    (if (or (not adjacent) (in-vec? adjacent visited))
      (conj match-words string)
      (recur remaining-adjacent (conj visited adjacent) (str string (get-in grid adjacent)))
      #_(recur (adjacent-coords adjacent) [adjacent] "")
))))


#_(list-matches [[1 2][3 4]] [1 0])

(print (grid-coords [[1 2][3 4]]))
