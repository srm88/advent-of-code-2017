(ns advent-of-code.stream)

(def debug? false)

(defn debug
  [& args]
  (when debug?
    (apply println args)))

(defn check
  [f expected arg]
  (println "input    " (apply str (take 70 (str arg))))
  (println (str "expected: " expected " got: " (f arg))))

(defn read-input
  []
  (butlast (slurp "input-9.txt")))

;; ######
;; Part 1
;; ######

; < garbage
; ! in garbage cancel next char
; > close garbage
; { open group
; } close group

(defn score
  [ch-stream]
  (letfn [(normal
            [level score ch-stream]
            (case (first ch-stream)
              \< #(garbage level score (rest ch-stream))
              \} #(normal (dec level) (+ score level) (rest ch-stream))
              \{ #(normal (inc level) score (rest ch-stream))
              \, #(normal level score (rest ch-stream))
              nil score))
          (garbage
            [level score ch-stream]
            (case (first ch-stream)
              \! #(garbage level score (rest (rest ch-stream)))
              \> #(normal level score (rest ch-stream))
              nil score
              #(garbage level score (rest ch-stream))))]
    (trampoline normal 0 0 ch-stream)))

(def check-score (partial check score))

(when false
  (check-score 1 "{}")
  (check-score 6 "{{{}}}")
  (check-score 5 "{{},{}}")
  (check-score 16 "{{{},{},{{}}}}")
  (check-score 1 "{<a>,<a>,<a>,<a>}")
  (check-score 9 "{{<ab>},{<ab>},{<ab>},{<ab>}}")
  (check-score 9 "{{<!!>},{<!!>},{<!!>},{<!!>}}")
  (check-score 3 "{{<a!>},{<a!>},{<a!>},{<ab>}}")
  (check-score "?" (read-input)))

;; ######
;; Part 2
;; ######

(defn count-garbage
  [ch-stream]
  (letfn [(normal
            [g-count ch-stream]
            (case (first ch-stream)
              \< #(garbage g-count (rest ch-stream))
              nil g-count
              #(normal g-count (rest ch-stream))))
          (garbage
            [g-count ch-stream]
            (case (first ch-stream)
              \! #(garbage g-count (rest (rest ch-stream)))
              \> #(normal g-count (rest ch-stream))
              nil g-count
              #(garbage (inc g-count) (rest ch-stream))))]
    (trampoline normal 0 ch-stream)))

(def check-count-garbage (partial check count-garbage))

(when true
  (check-count-garbage 0 "<>")
  (check-count-garbage 17 "<random characters>")
  (check-count-garbage 3 "<<<<>")
  (check-count-garbage 2 "<{!>}>")
  (check-count-garbage 0 "<!!>")
  (check-count-garbage 0 "<!!!>>")
  (check-count-garbage 10 "<{o'i!a,<{i<a>")
  (check-count-garbage "?" (read-input)))
