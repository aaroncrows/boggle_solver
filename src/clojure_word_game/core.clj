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
      (map #(keyword (str %)) (seq curr))
      assoc :word curr)))))

(defn make-board
  "makes a game board of random letters"
  [s]
  (vec (for [x (range s)]
    (random-letter-vec s))))

(def board (make-grid 3))

(defn adjacent-coords
  [[x y]]
  (for [mod-y [-1 0 1] mod-x [-1 0 1]
          :when (not-every? #(= 0 %) [mod-x mod-y])]
          [(+ mod-y y)(+ mod-x x)]))

(defn each-adjacent
  [f grid coords]
  (def adjacent-coords (adjacent-coords coords))

  (loop [[dir & remaining-dirs] adjacent-coords]
    (if dir
      (do (f (get-in grid dir))
        (recur remaining-dirs))
      match)))

(each-adjacent #(print %) [[1 2 3][4 5 6][7 8 9]] [1 1])