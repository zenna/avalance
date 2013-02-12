; A set of program transforms

; Replace random expression
; Find a random expression
; Find its type
; try n times to find another one in the library

; 1. COUNT ELEMENTS FLATTEN
; 2. gET type
; 3. FIND OBJECT
; IF REPLACEMENT OBJECT HAS DEPENDENCIES RECURSE

; PROBLEMS:
; FINDING PLACE OF CHANGE
; STOPPING INFINITE RECURSION

(ns avalance.transforms)
(require 'avalance.helpers)

(defn replace-in-list [coll n x]
  (concat (take n coll) (list x) (nthnext coll (inc n))))

(defn replace-in-sublist [coll ns x]
  (if (seq ns)
    (let [sublist (nth coll (first ns))]
      (replace-in-list coll
                       (first ns)
                       (replace-in-sublist sublist (rest ns) x)))
    x))

(defn get-index-vector
  "Get an index vector e.g. [2 3 1] for a position in a nested list"
  [coll pos]
  (loop [coll coll pos pos i 0]
    (if (or (zero? pos) (empty? coll))
      [i]
      (if (list? (first coll))
        (let [inner-level (get-index-vector (first coll) pos)
              pos (- pos (sum inner-level))]
          (if (<= pos 0)
            (vec (concat i inner-level))
            (recur (first coll) pos (inc i))))
        (recur (rest coll) (dec pos) (inc i))))))

(defn change-random-branch
  [func lib]
  (let [flat-func (flatten func)
      rand-elem-pos lib
      index-vector (get-index-vector rand-elem-pos)]
      0))

(defn -main
  []
  (let [tester '(0 1 2 ( 3 4 (5) 6) 7)]
    (get-index-vector tester 3)))


(require 'avalance.transforms)
(use 'clojure.tools.trace)
(trace-ns 'avalance.transforms)
(-main)