s(ns ^{:doc "The avalanve algorithm"
      :author "Zenna Tavares"}
avalance.optimise
  (:use avalance.grammar)
  (:use clozen.helpers)
  (:require [clojure.math.combinatorics :as combo]))

; TODO
; Transformations need to be type consistent
; Need better planning, instead of depth first bread first search

; Write now I make m move based on transformations
; The things deficient with thsi are
; it eems like a lot of thee problems can be thought of as decision making processes, where there are a number of things i can do.  I am trying to construct some kind of object according to some specification.
; I can analyse the specification, analyse the object.
; It's like planning in the sense there are a numbe of actiosns I can put execute.  But it's different in the sesne that actions change the state of knowledge of my knowledge.  I'm not necessarily trying to compose a sequence of actiosn.
; THere are two things that change, the object I am trying to construct, and my knowledge.
; also like planning in the sense that I need to think a few steps ahead
;; There are no analysis functions, no room for thinking
;; How to incorporate something like the simplex method?
;; I want to get away from optimisationt.  Because cost functions hide a great deal
;; 

;TSP stuff
(def init-tour [[2 0.1] [0.8 0.3] [0.2 0.16] [0.267 0.61]])

(defn tour-length
  "total length"
  [tour & extra-args]
  (reduce + (between (fn [x y] (+ (sqr (- (nth x 0) (nth y 0)))
                       (sqr (- (nth x 1) (nth y 1))))) tour)))

; Initial stochastic gradient ascent search algorithm 
; Apply a set of transforms to an intitial soltuion
; Evaluate the quality of each one
; stochastically choose the best one
; repeat
(def program
  "Initial stochastic gradient ascent algorithm"
  {:as-list '(fn
  [cost-func init-sol transforms lib depth]
  (loop [sol-current init-sol step 20]
  (let [selected-transforms (repeatedly 5 #(rand-nth transforms))
        proposals ((apply juxt selected-transforms) sol-current)
        proposal-costs (map #(cost-func % lib depth) proposals)
        sol ((:reciprocal-categorical lib) proposals proposal-costs)
        pvar (println "cost" (cost-func sol lib depth))]
    
    (if (zero? step)
      sol
      (recur sol-current (dec step))))))
  
	:type-sig []
	:program-transforms [identity]})

(def lib
  "A library of functions since eval function dont have access to
  global env"
  {:rand-choice rand-choice
   :max-elements max-elements
   :reciprocal-categorical rand-nth-reciprocal-categorical})

; Structure for problems
(defstruct problem :transforms :init-sol :cost-func)
(def tsp (struct problem [shuffle, reverse] init-tour tour-length))
(def problems [tsp])

(defn eval-search
  "Evaluate search algoritm cost
   An algorithm is evaluated both on its performance over
   some range of problems
   But also on the future consequences of using that algorithm.
   That is, suppose this is my new current solution, where will we go?"
  [program, lib, depth]
  (let [eval-search-algo (eval (:as-list program))
        prob-sols (map (fn [prob] (eval-search-algo 
                                    (:cost-func prob)
                                    (:init-sol prob) 
                                    (:transforms prob)
                                    lib
                                    0))
                    problems)
        ; TODO- get scores for each SA
        prob-scores (lines (extract problems :cost-func) prob-sols)
        pvar (println "scores" prob-scores)
        sum-prob-scores (sum prob-scores)]
    (println "depth" depth)
    ; [cost-func, init-sol, transforms, lib, depth]
    
    (if (zero? depth)
      sum-prob-scores
      (+ sum-prob-scores
        (eval-search
        	(eval-search-algo eval-search program 
                            (:program-transforms program) lib (dec depth))
          lib 0)))))

(defn -main
  []
  ((eval (:as-list program)) (:cost-func tsp) (:init-sol tsp) (:transforms tsp) lib 0))

; (defn -main
; 	[]
; 	((eval (:as-list program))
;           eval-search program
;           (:program-transforms program)
;           lib 1)) 