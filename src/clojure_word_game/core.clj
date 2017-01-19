(ns clojure-word-game.core
  (:gen-class))

(require '[clojure.string :as s])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (slurp "/usr/share/dict/words"))

(defn generate-word-set
  []
  (filter
    #(= % (s/lower-case %))
    (s/split (slurp "/usr/share/dict/words")
    #"\n")))

#_(defn generate-trie
  [words]
  (def make-key (comp keyword str))
  (let [trie {}]
    (loop [[curr & left] words
            trie]
      (loop [[cur-char & remain-char] curr
              trie]

        (when (not (nil? cur-char))
          (do
            (when (nil? ((make-key cur-char) trie))
            (recur remain-char))))
    (if (nil? curr)
      trie
    (do
    (recur left)))))

))

(defn random-letter
  "Returns a random capital letter from A-Z"
  [& args]
  (-> 26 rand-int (+ 65) char str))

(defn random-letter-seq
  "Returns a string of random letters"
  [length]
  (map random-letter (range length)))

(defn make-trie
  [words]
  (loop [[curr & rest] words trie {}]
    (if (nil? curr)
     trie
     (recur rest (update-in trie
      (map #(keyword (str %)) (seq curr))
      assoc :word curr)))))

(defn make-board
  "makes a game board of random letters"
  [s]
  (vec (for [x (range s)]
    (vec (random-letter-seq s)))))

(def board (make-grid 3))



(defn adjacent
  [grid coords]
  ((get-in grid coords)))

#_(adjacent board [2 2])



