(ns clojure-word-game.trie-test
  (:require [clojure.test :refer :all]
            [clojure-word-game.trie :refer :all]))

(deftest trie-test
  (testing "Should return a map"
    (def test-trie (make-trie ["test"]))
    (is (instance? clojure.lang.PersistentArrayMap test-trie)))

  (testing "Should make a nested map with chars as keys"
    (def test-trie (make-trie ["test"]))
    (is (not (nil? (get-in test-trie [:t :e :s :t])))))

  (testing "Should store the word at the last node"
    (def test-trie (make-trie ["test"]))
    (is (= "test" (get-in test-trie [:t :e :s :t :word]))))

  (testing "Should make diverge for similar words"
    (def test-trie (make-trie ["test" "text"]))
    (is (and
          (not (nil? (get-in test-trie [:t :e :s])))
          (not (nil? (get-in test-trie [:t :e :x]))))))

  (testing "Should not store a word before the last character"
    (def test-trie (make-trie ["test"]))
    (is (nil? (get-in test-trie [:t :e :s :word])))))