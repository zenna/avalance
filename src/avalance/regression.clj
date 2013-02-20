; Symbolic Regression with good proposals
(ns avalance.regression
  (:require [clojure.math.combinatorics :as combo]))

(use 'avalance.equations)

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

; Given a map and a function, perform map on values and return map with same keys
(defn map-assoc [f m]
  (into {} (for [[k v] m] [k (f v)])))

; A study encapsulates the idea of manipulating variables and classifying the result
; Assume tests is a sequence of values like this [[1] [2] [3]]
  ; tests = {'double:data 'wicked:data test}
; results = {'doubled?:lambda-pred}
(defn study-func
  "This applies a coll of tests to a function and classifies results"
  [func tests results]
  ; raw-results is func applied to each test, of form {'test1:[1 2 3] ...}
  (let [raw-results (map-assoc (fn [x] (map #(apply func %1) x)) tests)]
        ; return map from testname to all valid results e.g. {'test1:[doubled?,zero?]}
        (map-assoc (fn [x] 
          (loop [loop-results results woop []]
          (if (empty? loop-results) woop
            (let [result (first loop-results)
                  result-applies (apply (val result) x)
                  ; If the result is true, then save the resultname
                  result-to-add ({true [(key result)] false []} result-applies)]
              (recur (rest loop-results) (concat woop result-to-add)))))) raw-results)))

; Evaluates a set of attributes with respect to set of studies
; Works by generating G functions for which these attributes hold
; Performing same tests on G functions where it is known attributes hold 
; Checking what fraction of results are consistent
; TODO-TO-ALPHA: HACK-GENERATOR! for predefined set of functions
(defn eval-attrs
  "Evaluate a coll of attributes with respect to a set of studies"
  [attrs studies all-tests all-results]
  (let [num-funcs-to-gen 3
        ; vector of generated functions with attribute constraints
        gend-funcs (gen-funcs attrs num-funcs-to-gen)
        filtered-tests (select-keys all-tests (keys studies))
        ; List (one for each func) of studies on these
        ; e.g [{'test1 ['doubled,zero] 'test2:[]} {...} ...]
        gend-studies (map #(study-func %1 filtered-tests all-results) gend-funcs)
        ok (println "STUIES" gend-studies)
        counts (loop [loop-gend-studies gend-studies
                      study-counts (map-assoc (fn [x] 1) studies)]
          (if (empty? loop-gend-studies)
              study-counts
              ; iterates through all G funcs and foreach check if test led to same result or not
              (recur
                (rest loop-gend-studies)  
                ; Check by merge-with on three maps, since they all have the same keys
                ; fn checks if result of experiment same, if so increment study counts
                ; study-counts)))]
                (merge-with
                  (fn [match? study-count]
                    (if match?
                        (inc study-count)
                        study-count))
                  (merge-with
                    (fn [studies gend-studies]
                      (if (nil? (some #{studies} gend-studies))
                          false
                          true))
                    studies (first loop-gend-studies))
                  study-counts))))]
    ; To get joint probabiliorty, normalise (map) then  * (reduce) (assuming independence)
    (reduce * (map #(/ %1 num-funcs-to-gen) (vals counts)))))

(def tests
  {'increase [[1] [10]]
   'decrease [[10] [1]]
   'zero [[10]]})

(def results
  {'increased? (fn [& args] (> (last args) (first args)))
   'decreased? (fn [& args] (< (last args) (first args)))
   'non-zero? (fn [& args] (not (zero? (last args))))
   'zero? (fn [& args] (zero? (last args)))})

(defn test-func
  [x]
  (/ 1 (+ x 3)))
; 
(def test-studies {'increase ['decreased? 'non-zero?]
  'decrease ['increased? 'non-zero?]})

(defn -main
  []
  (eval-attrs x-in-denom? test-studies tests results))

; ; TARGET-FUNCTION ------------------------------------------
; (defn coloumbs-law
;   [q1 q2 r]
;   (/ (* 1.2342 (* q1 q2))
;           (* r r)))

; (defn cost-func
;   [proposal])

; (defn -main
;   []
;   (let [densities (gen-uniform-likelihoods attributes tests tests)
;         plus-density (densities 'plus)]
;     (plus-density {'linear 0 'denominator 0 'test 'much-less-than 'result 'much-less-than})))


; (require 'avalance.regression)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.regression)
(-main)