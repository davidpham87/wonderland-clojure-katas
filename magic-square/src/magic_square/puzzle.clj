(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as mc]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn make-square [values]
  (mapv vec (partition 3 values)))


(defn row-sum [m]
  (map #(apply + %) m))


(defn col-sum [m]
  (row-sum (apply map vector m)))


(defn diag-sum [m]
  (->> [#(get-in m [% %]) #(get-in m [% (- 2 %)])]
       (map #(map % (range 3)))
       (map #(apply + %))))


(defn perfect-square? [matrix]
  (apply = (reduce concat ((juxt row-sum col-sum diag-sum) matrix))))


(defn magic-square
  ([values] (magic-square values 0))
  ([values n-permutation]
   (let [matrix (make-square values)]
     (if (perfect-square? matrix)
       matrix
       (recur (mc/nth-permutation values n-permutation) (inc n-permutation))))))

(def m [[1.0 1.5 2.0]
        [2.5 3.0 3.5]
        [4.0 4.5 5.0]])
