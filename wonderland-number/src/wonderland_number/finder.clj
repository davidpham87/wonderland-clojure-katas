(ns wonderland-number.finder)

;; - It has six digits
;; - If you multiply it by 2,3,4,5, or 6, the resulting number has all
;;   the same digits in at as the original number.  The only difference
;;   is the position that they are in.

(def candidates
  (take-while #(< % (Math/pow 10 6)) (drop (Math/pow 10 5) (range))))

(defn same-digits [x y]
  (apply = (map (comp set str) [x y])))

(defn valid? [x]
  (every? identity (map #(same-digits x (* x %)) (range 2 7))))

(defn wonderland-number []
  ;; calculate me
  (first (filter valid? candidates)))
