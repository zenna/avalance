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