(ns clojure-word-game.trie-test
  (:require [clojure.test :refer :all]
            [clojure-word-game.trie :refer :all]))

(deftest trie-tests
  (testing "Should return a map"
    (def test-trie (make-trie ["two" "words"]))
    (is (instance? clojure.lang.PersistentArrayMap test-trie))))