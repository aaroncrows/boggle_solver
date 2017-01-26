(ns clojure-word-game.util)

(defn str-to-keys
  "Turns a string into a seq of keywords"
  [string]
  (map #(-> % str keyword) string))