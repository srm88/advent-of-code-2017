(ns advent-of-code.spiral)

(def input 347991)

(defn check
  [f expected arg]
  (println (take 70 (str arg)))
  (println (str "expected: " expected " got: " (f arg))))

;; down-then-left yields ... odd squares? 1, 9, 25, 49
;;37                    31
;;   17  16  15  14  13 30
;;   18   5   4   3  12
;;   19   6   1   2  11
;;   20   7   8   9  10
;;   21  22  23  24  25 26
;;43                    49

;; First step is to find which level it's in.
(def odd-squares (->> (range) (filter odd?) (map #(* % %))))

(defn which-level
  "1 is the 0th level, 2-9 is the 1st level, 10-25 is the 2nd level, etc."
  [n]
  (->> odd-squares
       (map-indexed (fn [level bottom-right]
                      (when (>= bottom-right n)
                        level)))
       (some #(when (some? %) %))))

;; The answer will be equal to the level if our number is in the center of
;; any side. E.g. the answer to 2,4,6,8 is 1; to 11,15,19,23 is 2, ...
;; More specifically, our answer is the number's level plus the number of steps
;; from its side's center.
;; So 25 is 2 (the level) + 2 (steps from 23) = 4.

(defn side-centers
  "1 -> [8 6 4 2], 2 -> [23 19 15 11], ..."
  [level]
  (let [bottom-right (nth odd-squares level)
        step (* level 2)
        start (- bottom-right level)] ;; this is the center of the bottom side
    (take 4 (iterate #(- % step) start))))

(defn closest-to
  [nums n]
  (->> nums
       (sort-by #(Math/abs (- n %)))
       first))

(defn spiral-steps
  [n]
  (if (= n 1)
    0
    (let [level (which-level n)
          closest-side-center (closest-to (side-centers level) n)]
      (-> closest-side-center
          (- n)
          (Math/abs)
          (+ level)))))

(def check-1 (partial check spiral-steps))

(check-1 0 1)
(check-1 3 12)
(check-1 2 23)
(check-1 31 1024)
(check-1 "?" input)
