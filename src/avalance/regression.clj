; Symbolic Regression with good proposals
(ns avalance.regression
  (:require [clojure.math.combinatorics :as combo]))

; ATTRIBUTES AND FRAGMENTS ------------------------------------------
(defn contains-frag-plus
  [expression bindings]
  )

(def attributes {'contains-frag-plus contains-frag-plus
  'contains-frag-line contains-frag-plus
  'contains-frag-denominator contains-frag-plus})

(defn x-x-binding
  [arg-name]
  'x)

(def bindings {'x-x-binding x-x-binding})

; Fragments are quoted expressions of 
; primitive function e.g. +, -, map
; the fragments structure is a map of quoted names to these expressions
(def fragments {'plus '(+ x ?)
				'line '(+ (* (? x) ?))
        'denominator '(/ (x ?))})

; TESTS ------------------------------------------
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

; INFERENCE ------------------------------------------
; Sample fragment from prior
; (defn gen-funcs
;   "Generate functions"
;   [fragments likelihoods priors]
;   ; Sample fragment from posterior
;   ; keep sampling as long as these fragments are true
;   )

; Builds a condition probability table of a fragment
; i.e. probability of fragment being present in expression
; given all combinations of other fragments being present or not
; in expression and all combinations of tests and results
; Sets all these uniformly
(defn gen-cond-prob-table
	[attributes bindings tests results]

  ; Since condition probabity, w
	(let [; *-spaces denotes the space of values this R.V or set of RVs can take
        ;Count is decremented since table size should not include unconditioned fragment
        attribute-spaces (take (dec (count attributes)) (repeat [0 1]))
        bindings-spaces (keys bindings)
        tests-spaces (keys tests)
        results-spaces (keys results)

        ; Find the cartesian product of all the spaces, i.e. build the table
        all-combinations (apply combo/cartesian-product
                                  (concat attribute-spaces [tests-spaces] [bindings-spaces] [results-spaces]))]
        all-combinations))

; a likelihood is a function from
; fragments is a map from quoted attribute name to quoted expressions
; frag1-name ffrag1-present? fragN-name fragN-present? experiment outcome -> likelihood 
; (+ x ?) (+ (* (m x) c) -> [lambda lambda]
(defn gen-uniform-likelihoods
  [attributes bindings tests results]
  (let [cond-prob-table (gen-cond-prob-table attributes bindings tests results)
        
        ; Uniform density, so 1/total number of comninations
        uniform-likelihood (/ 1 (count cond-prob-table))
        
        ; Takes a attribute name and returns a likelihood as a lambda
        make-likelihood (fn [attribute-name]
          (let [remapped-table (map #(zipmap (concat (keys (dissoc attributes attribute-name))
                                          '(test result)) %1)
                                  cond-prob-table)
                density (zipmap remapped-table (take (count remapped-table) (repeat uniform-likelihood)))]
            (fn [assignments] (density assignments))))]

    ; Return map with same keys as attributes but with value as likelihood density lambda
    (into {} (for [[k v] attributes] [k (make-likelihood k)]))))


; uniform-likelihood (/ 1 (count cond-prob-table))
; For every attribute
; create a function which maps
(defn symbolic-regression
  [target-function attributes bindings tests results cost-func]
  (let [likelihoods (gen-uniform-likelihoods attributes bindings tests results)]
  	))

  ; 1. Create uniform likelihood function for each attribute
  ; 2. for each attribute:
  ;   for every combination of (all other fragmet):
  ;     generate N functions with or without other fragments and this fragment
  ;     ; PROBLEM, HOWCAN I BE SURE THAT after compoisiton a function not inteded to be there is not there
  ;     for all combinations of tests, outcomes
  ;       hits = num times when tests = T, outcome = O.
  ;       likelihood = hits/ #tests

  ; 3. use proposals = gen-funcs(fragments new-likelihoods)
  ; 4. map proposals cost-func
  ; 5. get best

; Generate num functions which adhere to attribute constraints
(defn gen-funcs
  "Generate num functions which adhere to attribute constraints"
  [attrs number])

; A study encapsulates the idea of manipulating variables and classifying the result
(defn study-func
  "This applies a coll of tests to a function and classifies results"
  [func tests results]
  ())

(defn eval-attrs
  "Evaluate a coll of attributes on a coll of study"
  [attrs studies]
  (let [num-funcs-to-gen 100
        gen-funcs (gen-with-attrs attrs)
        old-experiments (map #(study-func %1 tests results) gen-funcs)]))

So the algorithm is something like 


#we need to generate functions which adhere to these constraints
def generate-with-constraints(attrs-set,100):
  How to use this?

# cooerce variables
def perform-experiments(func, tests, results):
  # apply all the tests to the func
  op-sequence-results = map(func, tests)

  # to the output sequences, apply the result
  experiments
  for sequence in op-sequence-results:
    results-output = juxt(sequence, results)
    
    # filter-results e.g. reduced, dramatically-reduced, isZero
    filter-results = filter(results-output, =true)

    # experiments = [double:reduced, doubled:dramatically-reduced,...]
    all-experiments.concat(experiment)

  return experiments

; generates a set of attributes which are likely to be part of the expression
def evaluate-attr-set(attrs-set, experiments):
  num-funcs-to-gen = 100

  # Generate 100 functions which adhere to the attribute constraints
  # TODO! HARD
  gs = generate-with-constraints(attrs-set,100)

  old-experiments = map(perform-experiments, gs)
  
  # Now for each experiment in experiment, assume independence:
  for experiments in experiment:
    test = experiment.test
    result = experiment.result

    old-experiments-of-same-test = filter(old-experiments, old-experiment.test == test)
    num-same-test = count(old-experiments-of-same-test)

    same-result = filter(old-experiments-of-same-test, old-experiment.result = result)

    return fraciton


def optimise-attribute-set(experiments):
  # Somehow select a set of attributes
  attribute-set = select-attribute-set()
  evaluate-attr-set(attr-set)
  

; TARGET-FUNCTION ------------------------------------------
(defn coloumbs-law
  [q1 q2 r]
  (/ (* 1.2342 (* q1 q2))
          (* r r)))

(defn cost-func
  [proposal])

(defn -main
  []
  (let [densities (gen-uniform-likelihoods attributes tests tests)
        plus-density (densities 'plus)]
    (plus-density {'linear 0 'denominator 0 'test 'much-less-than 'result 'much-less-than})))


; (require 'avalance.regression)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.regression)
(-main)