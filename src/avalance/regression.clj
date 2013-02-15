(ns avalance.regression
  (:require [clojure.math.combinatorics :as combo]))

; Fragments are quoted expressions of 
; primitive function e.g. +, -, map
; the fragments structure is a map of quoted names to these expressions
(def fragments {'plus '(+ x ?)
				'linear '(+ (* (m x) c))
        'denominator '(/ (x ?))})

; an equality to use for floating point arithmetic
(defn tolerant=
  [x y]
  (< (* (x x) 0.001)))

; A test is a predicate on a sequence of test conditions
(defn much-greater-than
	[x0 x1]
	(> x1 (* x0 1.5)))

(defn much-less-than
  [x0 x1]
  (< x1 (* x0 1.5)))

; tests structure is a map from quoted test name to test
(def tests {'much-greater-than much-greater-than
            'much-less-than much-less-than
            'zero? zero?})

; Sample fragment from prior
(defn gen-funcs
  "Generate functions"
  [fragments likelihoods priors]
  ; Sample fragment from posterior
  ; keep sampling as long as these fragments are true
  )

; Builds a condition probability table of a fragment
; i.e. probability of fragment being present in expression
; given all combinations of other fragments being present or not
; in expression and all combinations of tests and results
; Sets all these uniformly
(defn gen-cond-prob-table
	[fragments tests results]

  ; Since condition probabity, w
	(let [; *-spaces denotes the space of values this R.V or set of RVs can take
        ;Count is decremented since table size should not include unconditioned fragment
        fragment-spaces (take (dec (count fragments)) (repeat [0 1]))
        tests-spaces (keys tests)
        results-spaces (keys results)

        ; Find the cartesian product of all the spaces, i.e. build the table
        all-combinations (apply combo/cartesian-product
                                  (concat fragment-spaces [tests-spaces] [results-spaces]))]
        all-combinations))

; a likelihood is a function from
; fragments is a map from quoted fragment name to quoted expressions
; frag1-name ffrag1-present? fragN-name fragN-present? experiment outcome -> likelihood 
; (+ x ?) (+ (* (m x) c) -> [lambda lambda]
(defn gen-uniform-likelihoods
  [fragments tests results]
  (let [cond-prob-table (gen-cond-prob-table fragments tests results)
        
        ; Uniform density, so 1/total number of comninations
        uniform-likelihood (/ 1 (count cond-prob-table))
        
        ; Takes a fragment name and returns a likelihood as a lambda
        make-likelihood (fn [fragment-name]
          (let [remapped-table (map #(zipmap (concat (keys (dissoc fragments fragment-name))
                                          '(test result)) %1)
                                  cond-prob-table)
                density (zipmap remapped-table (take (count remapped-table) (repeat uniform-likelihood)))]
            (fn [assignments] (density assignments))))]

    ; Return map with same keys as fragments but with value as likelihood density lambda
    (into {} (for [[k v] fragments] [k (make-likelihood k)]))))


; uniform-likelihood (/ 1 (count cond-prob-table))
; For every fragment
; create a function which maps
  
(defn symbolic-regression
  [target-function fragments tests results cost-func]
  (let [likelihoods (gen-uniform-likelihoods fragments tests results)]
  	))

  ; 1. Create uniform likelihood function for each fragment
  ; 2. for each fragment:
  ;   for every combination of (all other fragmet):
  ;     generate N functions with or without other fragments and this fragment
  ;     ; PROBLEM, HOWCAN I BE SURE THAT after compoisiton a function not inteded to be there is not there
  ;     for all combinations of tests, outcomes
  ;       hits = num times when tests = T, outcome = O.
  ;       likelihood = hits/ #tests

  ; 3. use proposals = gen-funcs(fragments new-likelihoods)
  ; 4. map proposals cost-func
  ; 5. get best


(defn coloumbs-law
  [q1 q2 r]
  (/ (* (* 1.2342) (* q1 q2))
          (* r r)))

(defn cost-func
  [proposal])

(defn -main
  []
  (let [densities (gen-uniform-likelihoods fragments tests tests)
        plus-density (densities 'plus)]
    (plus-density {'linear 0 'denominator 0 'test 'much-less-than 'result 'much-less-than})))


; (require 'avalance.regression)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.regression)
(-main)