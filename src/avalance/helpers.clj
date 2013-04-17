(ns avalance.helpers)

;Helprs
(defn between
  "Applies f(x,y) to list[n,n+1] to create a list of size count - 1"
  [f, coll]
  (if (= (count coll) 1)
    []
    (concat [(f (first coll) (second coll))] (between f (rest coll)))))
 
(defn lines
	"returns [(f0 v0),...,(fn vn)]"
		[funcs values]
		(if (= (count values) 0)
			[]
			(concat [((first funcs) (first values))]
				(lines (rest funcs) (rest values)))))

;FIXME : currently returning only first
(defn max-elements
  "Returns elements of coll where f(elements) is maximal"
  [f coll]
  (let [mapped-coll (map f coll)
        max-val (apply max mapped-coll)
        best-index (.indexOf mapped-coll max-val)]
    [(nth coll best-index)]))

(defn sum
  [coll]
  (reduce + coll))

(defn rand-bool
  "Return uniform over true,false"
  []
  (= (rand-int 2) 1))

; (defn rand-choice
;   "Choice element from coll"
;   [coll]
;   (nth coll (rand-int (count coll))))

(defn rand-choice
  "Choice element from coll"
  [coll]
  (let [i (rand-int (count coll))]
    (do 
      (nth coll i))))

; TODO
(defn rand-choice-weighted
  "Choice element from coll"
  [coll weights]
  (nth coll (rand-int (count coll) 1)))

(defn square
	"x^2"
	[x]
	(* x x))

; an equality to use for floating point arithmetic
(defn tolerant=
  [x y]
  (< (* (x x) 0.001)))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn memoize-n
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of  the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f n]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

; Returns map of key-list to element at that place .e.g {[1 2 1] 'y, ...}
(defn coll-to-keys
  "Find nested keys to elements in map, ignore whose where ignore-elem is true"
  [coll ignore-elem?]

  ((fn breadth-first [coll all-keys base-keys pos]
          ; (println "eq" equation "all-keys" all-keys "base-keys" base-keys "pos" pos)
          (cond
            (empty? coll)
            all-keys

            ; Don't add if we want to ignore it
            (ignore-elem? (first coll) pos)
            (recur (rest coll) all-keys base-keys (inc pos))

            ; If it's a list add both the list AND recurse on the innards of the list
            (list? (first coll))
            (let [list-key {(conj base-keys pos) (first coll)}
                  inner-keys (breadth-first (first coll) all-keys (conj base-keys pos) 0)]
              (recur (rest coll) (merge list-key inner-keys) base-keys (inc pos)))

            :else
            (recur (rest coll)
                   (merge all-keys {(conj base-keys pos) (first coll)})
                   base-keys
                   (inc pos))))

  coll {} [] 0))

; (deftest coll-to-keys-test
;   (let [data '(= (+ y 2) (+ (sin (/ x 2)) 3))
;         expected-result {[2 1] '(sin (/ x 2)), [1] '(+ y 2), [1 2] 2, [1 1] 'y,
;                          [2 1 1 1] 'x, [2 1 1 2] 2, [2 1 1] '(/ x 2),
;                          [2 2] 3, [2] '(+ (sin (/ x 2)) 3)}]
;     (is (= (coll-to-keys data (fn [elem pos] (zero? pos))) expected-result))))

(defn replace-in-list [coll n x]
  (concat (take n coll) (list x) (nthnext coll (inc n))))

(defn replace-in-sublist [coll ns x]
  (if (seq ns)
    (let [sublist (nth coll (first ns))]
      (replace-in-list coll
                       (first ns)
                       (replace-in-sublist sublist (rest ns) x)))
    x))