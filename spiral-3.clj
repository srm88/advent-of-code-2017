(ns advent-of-code.spiral)

(def debug? false)

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

(defn debug
  [& args]
  (when debug?
    (apply println args)))

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

(when false
  (check-1 0 1)
  (check-1 3 12)
  (check-1 2 23)
  (check-1 31 1024)
  (check-1 "?" input))

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23  24  25

;; Always add the previous value
;; Immediately after a corner add the previous-previous value
;; Cases for adding in numbers from the previous level:
;;   * corner: add matching corner from previous level
;;   * fourth corner: also add first number in the same level!
;;   * new level: previous + first number in now-old level
;;   * corner+1: step in one level + advance one
;;   * corner-1: step in one level + retreat one
;;   * others: step in one level + advance one + retreat one
;;
;; Need to remember the previous level and current level
;; How to index into the previous level based on the current?

(defn from-prev-side
  [old-ring ring which-side]
  ;; Sum the values in pos, pos-1, pos+1 from the previous side, using 0
  ;; for any out of bounds access.
  (let [pos (count (which-side ring))
        side (which-side old-ring)]
    (->> [pos (inc pos) (dec pos)]
         (map #(nth side % 0))
         (reduce +))))

;; Hard-code the first square, which is four "sides" each of a single
;; number. This removes the need to special case the ring that surrounds 1.
(def inner-ring {:right [1N]
                 :top [1N]
                 :left [1N]
                 :bottom [1N]})

(def next-side {:right :top
                :top :left
                :left :bottom
                :bottom :new-level})

(def new-ring {:right []})

(defn spiral-sum*
  [n]
  (if (= n 1)
    1
    ;; Model walking the spiral with a FSM, which is awkward in clojure.
    ;; new-level: the state for the first number after a completed ring.
    ;;            Begins on the right side, at position 1 (leaving 0 for
    ;;            the ring's final number). Transitions to 'side' except
    ;;            the very first new-level which jumps straight to 'corner'.
    (letfn [(new-level
              [old-ring step n]
              (let [value (from-prev-side old-ring new-ring :right)
                    old-ring* (update old-ring :bottom conj value)
                    ring (update new-ring :right conj value)
                    n* (dec n)]
                (debug "new level (step " step ") -> " value)
                (if (= n* 0)
                  value
                  (if (= 2 step)
                    #(corner old-ring* step :right ring n*)
                    #(side old-ring* step :right ring (- step 2) n*)))))
            ;;
            ;; side: state for walking in a straight line before reaching a
            ;;       corner. Transitions either to the next position in 'side'
            ;;       or to the side's 'corner'.
            ;;
            (side
              [old-ring step which-side ring til-corner n]
              (let [from-prev (from-prev-side old-ring ring which-side)
                    value (+ (last (which-side ring))
                             from-prev)
                    ring* (update ring which-side conj value)
                    til-corner* (dec til-corner)
                    n* (dec n)]
                (debug "side " which-side " prev " (last (which-side ring)) " prev-side " (which-side old-ring) " = " from-prev " -> " value)
                (if (= n* 0)
                  value
                  (if (= til-corner* 0)
                    #(corner old-ring step which-side ring* n*)
                    #(side old-ring step which-side ring* til-corner* n*)))))
            ;;
            ;; corner: this function handles turning a corner in the spiral.
            ;;         Transitions either to the next 'side', or 'new-level'
            ;;         for a ring's fourth corner.
            ;;
            (corner
              [old-ring step which-side ring n]
              (let [pre-corner (last (which-side ring))
                    value (+ pre-corner
                             (from-prev-side old-ring ring which-side))
                    ;; Finish current side with corner value
                    ring* (update ring which-side conj value)
                    n* (dec n)]
                (debug "corner of " which-side ", prev " pre-corner ", prev-side " (which-side old-ring) " -> " value)
                (if (= n* 0)
                  value
                  (if (= which-side :bottom)
                    #(new-level
                       ;; The final corner, in retrospect, starts the first side.
                       ;; E.g. the right side of level 2 after finishing level 2 is [25 1 2]
                       (update ring* :right (partial cons value))
                       (+ step 2)
                       n*)
                    (let [which-side* (next-side which-side)]
                      #(side
                         ;; Insert the pre-corner number into the next side of the old ring.
                         ;; E.g. If we're at this corner
                         ;;
                         ;;             59 <--
                         ;;  5   4   2  57
                         ;; 10   1   1  54
                         ;; 11  23  25  26
                         ;;
                         ;; We're going from the right to the top, old-ring top
                         ;; is [2 4 5], but prepending the pre-corner number like [57 2 4 5]
                         ;; ensures it is added into the next sum (122), which would other-
                         ;; wise only add 59 to 2.
                         (update old-ring which-side* (partial cons pre-corner))
                         step
                         which-side*
                         (assoc ring* which-side* [value])
                         (dec step)
                         n*))))))]
      #(new-level inner-ring 2 (dec n)))))

(defn spiral-sum
  [n]
  (trampoline spiral-sum* n))

(defn larger-spiral-sum-than
  [sum]
  (->> (range)
       (map inc)
       (map spiral-sum)
       (some #(if (> % sum) %))))

(def check-2 (partial check spiral-sum))

(when true
  (check-2 1 1)
  (check-2 1 2)
  (check-2 2 3)
  (check-2 4 4)
  (check-2 5 5))

(when true
  (check-2 10 6)
  (check-2 11 7)
  (check-2 23 8)
  (check-2 25 9)
  (check-2 26 10)
  (check-2 54 11)
  (check-2 57 12)
  (check-2 59 13)
  (check-2 122 14)
  (check-2 133 15)
  (check-2 142 16)
  (check-2 147 17)
  (check-2 304 18)
  (check-2 330 19)
  (check-2 351 20)
  (check-2 362 21)
  (check-2 747 22)
  (check-2 806 23)
  (check larger-spiral-sum-than "?" input))
