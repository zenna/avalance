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