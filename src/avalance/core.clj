(ns avalance.core)

(require 'avalance.helpers)



; BIG TODO - be able to generate functions and evaluate them
; Transformations need to be type consistent
; Syntactically correct
; Ensure that every tranformation provides the correct number of arguments
; That they are type consisten

;TSP stuff
(def init-tour [[2 0.1] [0.8 0.3] [0.2 0.16] [0.267 0.61]])

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