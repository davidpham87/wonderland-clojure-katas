(ns tiny-maze.solver)

;; test
(def maze [[:S 0 0]
           [1  0 1]
           [1  0 :E]])

(defn find-key [maze key]
  (->> maze
       (map vector (range))
       (filter (fn [[i r]] (some #(= key %) r)))
       first
       ((fn [[i x]] [i (.indexOf x key)]))))

(defn find-neighbour [maze path [r c]]
  (->> (for [i [-1 0 1]
             j [-1 0 1]
             :let [pos [(+ i r) (+ j c)]]
             :when (= 1 (Math/abs (- i j)))]
         (when (#{0 :E :S} (get-in maze pos 1))
           {:path (conj path pos) :pos pos}))
       (keep identity) vec))

(defn update-to-visit [pos to-visit visited maze path]
  (->> pos (find-neighbour maze path) (filter #(not (visited (:pos %))))
       (concat to-visit) vec))

(defn breadth-first-search [maze pos-begin pos-end path to-visit visited]
  (if-not (= pos-begin pos-end)
    (let [to-visit-new (update-to-visit pos-begin to-visit visited maze path)
          node (first to-visit-new)
          pos (:pos node)]
      (recur maze pos pos-end (:path node) (rest to-visit-new)
             (conj visited pos)))
    path))

(defn solve-maze
  ([maze] (solve-maze maze (find-key maze :S) (find-key maze :E) [[0 0]] [] #{}))
  ([maze pos-begin pos-end path to-visit visited]
   (let [final-path (breadth-first-search
                     maze pos-begin pos-end path
                     to-visit visited)]
     (reduce #(assoc-in %1 %2 :x) maze final-path))))
