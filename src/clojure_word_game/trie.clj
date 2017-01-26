(ns clojure-word-game.trie
  (:require [clojure-word-game.util :refer [str-to-keys]]))

(defn make-trie
  "Generates a trie from a list of words"
  [words]
  (loop [[curr & rest] words trie {}]
    (if (nil? curr)
     trie
     (recur rest
      (update-in trie
      (str-to-keys curr)
      assoc :word curr)))))