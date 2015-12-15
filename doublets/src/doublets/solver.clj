(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn n-diff-characters [x y]
  (->> [x y]
       (map #(map identity %))
       (apply map not=)
       (map #(if % 1 0))
       (apply + (Math/abs (- (.length x) (.length y))))))


(defn find-new-neighbours [word candidates path visited]
  (->> candidates
       (filter #(= 1 (n-diff-characters % word)))
       (filter (complement visited))
       (mapv #(assoc {:path (conj path)} :word %))))


(defn breadth-first-search
  ([begin end]
   (breadth-first-search begin end [begin] [] #{}
                         (filter #(= (count %) (count begin)) words)))
  ([begin end path to-visit visited candidates]
   (if (= begin end)
     path
     (let [neighbour (find-new-neighbours begin candidates path visited)
           new-to-visit (concat to-visit neighbour)
           node (first new-to-visit)]
       (breadth-first-search (:word node) end (conj (:path node) (:word node))
                             ((comp vec rest) new-to-visit)
                             (conj visited (:word node)) candidates)))))

(defn doublets [word1 word2]
  "make me work"
  (if (not= (count word1) (count word2))
    []
    (breadth-first-search word1 word2)))
