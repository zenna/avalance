(ns avalance.core)

; BIG TODO - be able to generate functions and evaluate them
; Transformations need to be type consistent
; Syntactically correct
; Ensure that every tranformation provides the correct number of arguments
; That they are type consisten

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

(defn max-elements
  "Returns elements of coll where f(elements) is maximal"
  [f coll]
  (let [max-val (apply max (map f coll))]
    (filter (fn [x] (= (f x) max-val)) coll)))

(defn sum
  [coll]
  (reduce + coll))

(defn rand-bool
  "Return uniform over true,false"
  []
  (= (rand-int 2) 1))

(defn rand-choice
  "Choice element from coll"
  [coll]
  (nth coll (rand-int (count coll))))

; TODO
(defn rand-choice-weighted
  "Choice element from coll"
  [coll weights]
  (nth coll (rand-int (count coll) 1)))

;TSP stuff
(def init-tour [[2 0.1] [0.8 0.3] [0.2 0.16] [0.267 0.61]])

(defn square
	"x^2"
	[x]
	(* x x))

(defn tour-length
  "total length"
  [tour & extra-args]
  (reduce + (between (fn [x y] (+ (square (- (nth x 0) (nth y 0)))
                       (square (- (nth x 1) (nth y 1))))) tour)))

; Initial stochastic gradient ascent search algorithm 
; Apply a set of transforms to an intitial soltuion
; Evaluate the quality of each one
; stochastically choose the best one
; repeat
(def program {:as-list '(fn
  [cost-func, init-sol, transforms, lib, depth]
  (let [make-step (fn [sol-current steps]
    ; Apply 5 random transformations to solution, and find lowest cost
    (let [selected-transforms (take 5 (repeatedly #((:rand-choice lib) transforms)))
          proposals ((apply juxt selected-transforms) sol-current)
          sol-best ((:rand-choice lib) ((:max-elements lib) #(cost-func %1 lib depth) proposals))]	
      
      ; Then repeat with best sol in this round as initial sol in next
      (if (zero? steps)
        sol-best
        (recur sol-best (dec steps)))))]

    (make-step init-sol 5)))
	:type-sig []
	:program-transforms [identity]})

(def lib {:rand-choice rand-choice :max-elements max-elements})

; Structure for problems
(defstruct problem :transforms :init-sol :cost-func)
(def tsp (struct problem [shuffle, reverse] init-tour tour-length))
(def problems [tsp])

(defn eval-search
  "Evaluate search algoritm cost"
  [search-algo, lib, depth]
  (let 
    ; Find problem solutions for each problem
    [eval-search-algo (eval (:as-list program))
    prob-sols (map (fn [prob] (eval-search-algo 
      (:cost-func prob)
      (:init-sol prob) 
      (:transforms prob)
      lib
      0)) problems)
    ; TODO- get scores for each SA
    prob-scores (lines (map #(:cost-func %1) problems) prob-sols)
    sum-prob-scores (sum prob-scores)]
    
    (if (zero? depth)     
      sum-prob-scores
      (+ sum-prob-scores (eval-search
      	(eval-search-algo eval-search search-algo (:program-transforms program) lib (dec depth))
      	lib (dec depth))))))

(defn -main
	[]
	((eval (:as-list program)) eval-search program (:program-transforms program) lib 1))