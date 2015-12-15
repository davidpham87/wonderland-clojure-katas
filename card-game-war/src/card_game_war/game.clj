(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn which-max [order vals]
  (->> vals
       (map (fn [i v] [i (.indexOf order v)]) (range))
       (apply max-key second)
       first))

(defn play-round [player1-card player2-card]
  (let [[s1 r1] player1-card
        [s2 r2] player2-card
        winner-number (which-max ranks [r1 r2])
        winner-suits (which-max suits [s1 s2])]
    (if (not= r1 r2)
      winner-number
      winner-suits)))

(defn refill-deck [current-deck fill-deck]
  (if (seq current-deck) [current-deck fill-deck] [(shuffle fill-deck) []]))


(defn play-game
  ([p1-cards p2-cards] (play-game p1-cards p2-cards [] []))
  ([p1-cards p2-cards p1-new-deck p2-new-deck]
   (let [t1 (concat p1-cards p1-new-deck)
         t2 (concat p2-cards p2-new-deck)]
     (if (and (seq t1) (seq t2))
       (let [[p1 p1-deck] (refill-deck p1-cards p1-new-deck)
             [p2 p2-deck] (refill-deck p2-cards p2-new-deck)
             cards [(first p1) (first p2)]
             winner (apply play-round cards)
             [new-cards-p1 new-cards-p2] (assoc [[] []] winner (shuffle cards))]
         (recur (rest p1) (rest p2) (concat p1-deck new-cards-p1) (concat p2-deck new-cards-p2)))
       (if (seq t1) 0 1)))))
