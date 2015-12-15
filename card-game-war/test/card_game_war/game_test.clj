(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (play-round [:spade 2] [:spade 5]) 1)))
  (testing "queens are higher rank than jacks"
    (is (= (play-round [:spade :queen] [:spade :jack]) 0)))
  (testing "kings are higher rank than queens"
    (is (= (play-round [:spade :king] [:spade :queen]) 0)))
  (testing "aces are higher rank than kings"
    (is (= (play-round [:spade :king] [:spade :ace]) 1)))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= (play-round [:spade :king] [:club :king]) 1)))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= (play-round [:diamond :king] [:club :king]) 0)))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= (play-round [:diamond :king] [:heart :king]) 1))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= (play-game [[:diamond :ace]] []) 0)))
  (testing "the player loses when they don't have good cards"
    (is (= (play-game [[:heart :ace]] [[:spade 2] [:spade 3] [:spade :king]]) 0)))
  (testing "the player wins when they have only good cards"
    (is (= (play-game [[:spade :queen] [:club 2] [:club 3]]
                      [[:heart :ace] [:diamond :ace] [:club :ace]]) 1))))
