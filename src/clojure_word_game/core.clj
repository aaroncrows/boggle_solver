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

#_(defn trie-word
  [word trie]
  (def make-key (comp keyword str))
  (loop [[curr & remaining-chars] word
          node {}]
          (print (assoc-in node (concat (seq remaining-chars) "word") word))
          (if curr
           (recur remaining-chars ((make-key curr)(assoc node (make-key curr) {})))
           (assoc trie :x node))))

#_(defn loop-chars
  [word]
  (loop [[curr & remaining] word]
    (print curr)
  (when curr (recur remaining))))

(update-in {}
  (map #(keyword (str %)) (seq "things"))
  assoc :word "things"
)

(defn make-trie
  [words]
  (loop [[curr & rest] words trie {}]
    (if (nil? curr)
     trie

     (recur rest (update-in trie
      (map #(keyword (str %)) (seq curr))
      assoc :word curr)))))

(defn make-grid
  [height]
  (take 20 (repeatedly #(-> 26 rand-int (+ 65) char str))))
