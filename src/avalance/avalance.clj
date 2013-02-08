(ns avalance.core)

(use 'clojure.contrib.math)

; BIG TODO - be able to generate functions and evaluate them
; Transformations need to be type consistent
; Syntactically correct
; Ensure that every tranformation provides the correct number of arguments
; That they are type consisten


; 1. Program transformations
; 2. eval search

;Helprs
(defn between
  "Applies f(x,y) to list[n,n+1] to create a list of size count - 1"
  [f, list]
  (if (= (count list) 1)
    []
    (concat [(f (first list) (second list))] (between f (rest list)))))

(defn max-element
  "Returns elements of list where f(elements) is maximal"
  [list f]
  (let [max-val (max (map f list))]
    (filter (fn [x] (= (f x) max-val)) list)))

(defn sum
  [list]
  (reduce + list))

(defn rand-bool
  "Return uniform over true,false"
  (= (rand-int 2) 1))

(defn rand-choice
  "Choice element from list"
  [list]
  (nth list (rand-int (- (count list) 1))))

(defn rand-choice-weighted
  "Choice element from list"
  [list weights]
  (nth list (rand-int (- (count list) 1))))

;TSP stuff
(def init-tour [[2 0.1] [0.8 0.3] [0.2 0.16] [0.267 0.61]])

(defn tour-length
  "total length"
  [tour]
  (reduce + (between (fn [x y] (+ (square (- (nth x 0) (nth y 0)))
                       (square (- (nth x 1) (nth y 1))))) tour)))

; Program Transforms - the object we are trying to transform may have is a set of functions
; and type information
(def std-typesigs {+ [int int], :map [f list]})
(def program {:funcs [] :type-sigs std-typesigs })

(defn replace-branch
  "This replacse a single branch of a program"
  [program]
  )

(def program-transforms [replace-branch])

; Structure for problems
(defstruct problem :transforms :init-sol :cost-func)
(def tsp (struct problem [shuffle, reverse] init-tour tour-length))
(def problems [tsp])

; Initial stochastic gradient ascent search algorithm 
; Apply a set of transforms to an intitial soltuion
; Evaluate the quality of each one
; stochastically choose the best one
; repeat
(defn sgc-search-algo
  "This does stochastic hill climbing"
  [cost-func, init-sol, transforms, depth]
  (let make-step (fn [sol-current steps]
    ; Apply 5 random transformations to solution, and find lowest cost
    (let [selected-transforms (take 5 (repeatedly #(rand-choice transforms)))
          proposals ((apply juxt selected-tranforms) sol-current)
          sol-best (max-element cost-func proposals)]
      
      ; Then repeat with best sol in this round as initial sol in next
      (if (zero? steps)
        sol-best
        (recur (dec sol-best steps))))))
    
    (make-step init-sol 5))

(defn eval-search
  "Evaluate search algoritm cost"
  [search-algo, depth]
  (let [MAXDEPTH 3
    ; Find problem solutions for each problem
    prob-sols (map (fn [prob] (search-algo 
      (:cost-func prob) 
      (:init-sol prob) 
      (:transforms prob))) problems)
    prob-scores ()
    score-probs (sum prob-scores)]
    
    (if (< depth MAXDEPTH)
      (+ score-probs (search-algo eval-search search-algo program-transforms (inc depth)))
      score-probs)))

; if I eval all the function then what will happen
; They will be callable by name, which is convenient
; But bad if I want parallel code

; Alternative is to prepend params to beginning and use in let statement
(defn avalance
  "Main loop - iteratively replace search-algo with better one"
  [num-cycles]
  (do
    (map eval (:funcs program))
    (if (zero? num-cycles)
      sgc(dec num-cycles))))

(defn -main
  []
  (avalance 10))