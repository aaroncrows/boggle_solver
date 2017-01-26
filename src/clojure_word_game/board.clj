(ns clojure-word-game.board)

(defn random-letter
  "Returns a random capital letter from A-Z"
  [& args]
  (-> 26 rand-int (+ 65) char str))

(defn random-letter-vec
  "Returns a vector of random letters"
  [length]
  (mapv random-letter (range length)))

(defn make-board
  "makes a game board of random letters"
  [s]
  (vec (for [x (range s)]
    (random-letter-vec s))))