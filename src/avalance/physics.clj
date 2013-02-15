(ns regression.core
  (:require [clojure.math.combinatorics :as combo]))

; Fragments are quoted expressions of 
; primitive function e.g. +, -, map
; the fragments structure is a map of quoted names to these expressions
(def fragments {'plus '(+ x ?)
				'linear '(+ (* (m x) c))})

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
            'much-less-than much-less-than})

; Sample fragment from prior
(defn gen-funcs
  "Generate functions"
  [fragments likelihood]
  ; Sample fragment from posterior
  ; keep sampling as long as these fragments are true
  )

; Generate likelihood tables
(defn gen-fragment-likelihoods
  [fragments]
  (let [test-funcs (gen-funcs fragments)]
    )
  map calculate)

'i need a function which will take a set of values'


'plus
{'a lambdaa 'b lambda b 'c 'lambdac'}
-> lambda {'a 0 'b 1} -> 3.2

{'f0 0}
(defn gen-uniform-likelihood
	[fragment-name fragments tests results]
	(let [filtered-fragments (disassoc fragments fragment-name)
        fragment-keys (keys filtered-fragments)
        fragment-spaces (take (count fragment-keys) (repeat [0 1]))
        tests-spaces (keys tests)
        results-spaces (keys results)
        all-combinations (apply combo/cartesian-product (concat fragment-spaces tests-spaces results-spaces))]

; a likelihood is a function from
; fragments is a map from quoted fragment name to quoted expressions
; frag1-name ffrag1-present? fragN-name fragN-present? experiment outcome -> likelihood 
; (+ x ?) (+ (* (m x) c) -> [lambda lambda]
(defn gen-uniform-likelihoods
  [fragments]
  (keys fragments)
  

(defn symbolic-regression
  [target-function fragments experiments outcomes cost-func]
  (let [likelihoods (gen-uniform-likelihoods fragments)]
  	(map fragments))

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
  )

(defn coloumbs-law
  [q1 q2 r]
  (/ (* (* 1.2342) (* q1 q2))
          (* r r)))

(defn cost-func
  [proposal])

(def (-main)
  []
  (symbolic-regression coloumbs-law experiments outcomes cost-func))



Question. How to store this matrix
Question. How to generate functiosn from fragments
Question. What are fragments

Is it easier to search for fragments or generate from fragments.

q (pf,x,y) = p(f)p(x)p(y|f.x)
= p(y)p(f,x|y)


1. TODO:
Design pghysics algorithm
Sigma machines
Look up csail professors

Physics
e.g. - force on a particle is 
F = ke |q1q2|/r2



; Generate N physics like functions
; Trying to learn that if I increase r and output goes down then r likely in denominator



- Vary individual variables, pairs of variables
- for each variation:
  - classify the variation by mapping with a bunch of proposition
  (map variation [increasing? decreasing?])
  - we have a program fragmets for each primitive function
  (* var missing) (/ var missing) (/ missing var)
  (sin missing) (+ var missing) (- var missing) (- missing var) ()
  - !! Evaluate how likely each fragment is to be part of the program
  given the data: HOW
  - use these probabilities to bias a stochastic grammar for programs and generate a familiy of progarms
  - test these programs against the data