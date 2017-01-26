(ns clojure-word-game.core
  (:require [clojure.string :as s]
            [clojure-word-game.util :refer [str-to-keys]]
            [clojure-word-game.board :refer [make-board]])
    (:gen-class))

(defn generate-word-set
  "Reads system word list and gives back a list less proper nouns"
  []
  (filter
    #(= % (s/lower-case %))
    (s/split (slurp "/usr/share/dict/words")
    #"\n")))
(def w (generate-word-set))

(defn make-trie
  "Generates a trie from a list of words"
  [words]
  (loop [[curr & rest] words trie {}]
    (if (nil? curr)
     trie
     (recur rest (update-in trie
      (str-to-keys curr)
      assoc :word curr)))))

(def trie (make-trie (generate-word-set)))

(defn coords-and-adjacent
  "takes a set of coords and a board size returns those coords and
   all coords within the boundaries of the board"
  [[y x] s]
  (for [mod-y [-1 0 1] mod-x [-1 0 1]
        :when (and (not (every? #(= 0 %) [mod-x mod-y]))
                   (<= 0 (+ y mod-y) (- s 1))
                   (<= 0 (+ x mod-x) (- s 1)))]
    [(+ mod-y y)(+ mod-x x)]))

(defn grid-coords
  "generates a set of all coordinates in a grid"
  [grid]
  (for [[y row] (map-indexed vector grid)
        [x val] (map-indexed vector row)]
  [y x]))

(defn is-prefix?
  "Takes a trie and a string and returns a boolean
   that reflects if the string is a valid prefix"
  [trie string]
  (->> string str-to-keys (get-in trie) boolean))

(defn all-words
  "takes a grid and coordinates and returns all valid words"
  [grid coords]
  (def grid-size (count grid))
  (defrecord Path [current string visited])
  (->Path coords "" #{})
  (loop [[path-map & remaining] [(->Path coords "" #{})]
         word-matches []]

      (if (or (not path-map))
        word-matches
        (let [new-maps
          (for [coords (coords-and-adjacent (:current path-map) grid-size)
                :let [{s :string v :visited c :current} path-map]
                :when (not (contains? v coords))]
                (->Path coords (str s (get-in grid c)) (into v [c])))
              word (:word (get-in trie (str-to-keys (:string path-map))))]
          (recur  (if (not remaining) new-maps (into remaining new-maps))
                  (if word (conj word-matches word) word-matches))))))

(def test-grid [["c" "a" "t"]
                ["b" "t" "o"]
                ["d" "g" "r"]])

(set
  (filter
    #(and (< 2 (count %)) (is-prefix? trie %))
    (flatten (for [square (grid-coords test-grid)]
              (all-words test-grid square)))))

