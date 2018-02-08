(ns advent-of-code.realloc)

(def debug? false)

(defn debug
  [& args]
  (when debug?
    (apply println args)))

(defn check
  [f expected arg]
  (println "input    " (apply str (take 70 (str arg))))
  (println (str "expected: " expected " got: " (f arg))))

(def input [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

;; ######
;; Part 1
;; ######

(defn redistribute
  [blocks]
  (let [to-redistribute (last (sort blocks))
        idx (.indexOf blocks to-redistribute)
        indexes (->> (range to-redistribute)
                     (mapv (partial + idx 1)) ;; shift by the position of the block after the one being broken up
                     (mapv #(mod % (count blocks))))] ;; mod each index so we wrap
    (as-> blocks b
      ;; Set the biggest bank to 0
      (assoc b idx 0)
      ;; Use reduce to inc every block in 'indexes'
      (reduce (fn [b* i] (update b* i inc)) b indexes))))

(defn find-loop
  [blocks*]
  (loop [seen #{}
         blocks blocks*
         cycles 1]
    (let [new-blocks (redistribute blocks)]
      (if (contains? seen new-blocks)
        {:cycles cycles
         :blocks blocks}
        (recur (conj seen new-blocks)
               new-blocks
               (inc cycles))))))

(defn how-many-cycles
  [blocks]
  (-> blocks find-loop :cycles))

(def check-cycles (partial check how-many-cycles))

(when true
  (check-cycles 5 [0 2 7 0])
  (check-cycles "?" input))

;; ######
;; Part 2
;; ######

(defn loop-size
  [blocks]
  (-> blocks find-loop :blocks find-loop :cycles dec))

(def check-loop-size (partial check loop-size))

(when true
  (check-loop-size 4 [0 2 7 0])
  (check-loop-size "?" input))

(use 'clojure.stacktrace)
(when *e
  (print-stack-trace *e))

