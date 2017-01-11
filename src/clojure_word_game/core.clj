(ns clojure-word-game.core
  (:gen-class))

(require '[clojure.string :as s])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (slurp "/usr/share/dict/words"))

(set (filter #(= % (s/lower-case %)) (s/split (slurp "/usr/share/dict/words") #"\n")))
