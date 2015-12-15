(ns fox-goose-bag-of-corn.puzzle)

;; Graph discovery algorithm with breadth first search

(def start-pos [[:fox :goose :corn :you] [:boat] []])
(def end-pos [[] [:boat] [:fox :goose :corn :you]])
(def end-pos-test [[:fox :corn] [:boat] [:goose :you]])
(def illegal-pos [[:fox :goose] [:boat] [:you :corn]])

(defn together-without-you?
  "Check if a given set m belongs to the vector of vector x and not with :you"
  [x m]
  (->> x (map set) set
       (filter #(and (every? % m) (nil? (:you %))))
       ;; (filter #(if (:boat %) (or (:you %) (= (count %) 1)) true))
       seq))

(defn authorized-state? [x]
  (let [ms [#{:goose :corn} #{:fox :goose} #{:boat :goose} #{:boat :fox}
            #{:boat :corn}]]
    (->> (map (partial together-without-you? x) ms)
         (some identity)
         not)))

(defn find-you
  "Find position and vector containing you"
  [x]
  (->> x (map set)
       (map vector (range 3))
       (filter #(-> % second :you))
       first))

(defn transportable-items
  "When finding the next move, one needs to know which items can be
  moved with :you. Return at least #{nil}"
  [v]
  (->> v (filter #{:fox :goose :corn}) (into #{}) (#(conj % nil))))

(defn clear-nil [x]
  (mapv #(into [] (remove nil? %)) x))

(defn move-items [from to what]
  ;; Not perfect, because we assume /what/ has unique items.
  ;; One should do that with hash-map and frequencies.
  ;; what should be a vector
  (->> [(remove (set what) from) (concat to what)]
       (mapv (partial into []))))

(defn update-vector [v & updates]
  (->> updates
       (reduce #(assoc %1 (first %2) (second %2)) v)
       (map (comp vec set))))

(defn find-neighbour
  "Neighbour are any element within the vector of :you (possibly nil) but
  not :boat and :you should go"
  [state]
  (let [[you-pos-old you-vec] (find-you state)]
    (for [you-pos-next ([[1] [0 2] [1]] you-pos-old)
          item (transportable-items you-vec)
          :let [[you-old you-new] (move-items you-vec (state you-pos-next) [:you item])]]
      (->> [[you-pos-next you-new] [you-pos-old you-old]]
           (apply update-vector state)
           clear-nil))))

(defn update-position-to-visit [actual-position to-visit visited path]
  (->> actual-position
       find-neighbour
       (filter #(and (authorized-state? %) (nil? (visited (map set %)))))
       vec
       (map #(assoc {:path path} :node %))
       (concat to-visit)
       vec))

(defn node-equal [xs]
  (->> xs (map #(map set %)) (apply =)))

(defn river-path
  ([] (river-path start-pos end-pos))
  ([begin end] (river-path begin end [begin] [] #{}))
  ([begin end path to-visit visited]
   (if (not (node-equal [begin end]))
     (do
       (let [new-to-visit (update-position-to-visit begin to-visit visited path)
             next-node-maps (first new-to-visit)
             next-node (:node next-node-maps)
             next-path (:path next-node-maps)] ;; keep only position not path
         (recur next-node end (conj next-path next-node) (rest new-to-visit)
                (conj visited (map set next-node)))))
     path)))

(def river-crossing-plan river-path)

;; new-path (conj path new-path)

#_(apply f [actual-position path to-visit visited])
#_(if (not= actual-position destination) () path)
;; (river-path start-pos end-pos)
