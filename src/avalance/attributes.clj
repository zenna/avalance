(ns ^{:doc "Attribute (feature) selection and learning.

            Attributes are functions of the data or model (data generated from
            model).
            They represent abstractions of the data which are important for
            tasks, in particular the task of choosing which model to extend.

            This module covers the constructing, selection and learning of
            attributes.

            There are three dimensiosn over solutions which we seek to explore,
            1. The criterion for a good feature
            2. The method for generating new features
            3. The search algorithm"
      :author "Zenna Tavares"}
avalance.attributes
  (:use avalance.grammar)
  (:use clozen.helpers)
  (:require [clojure.math.combinatorics :as combo]))

;;; Issues
;; 
; The values of the paramter coefficients are important; they may affect whether there is a single mode, whether it is sloping downwards and so on.
; This means we can not talk about the attributes of a model independently of its parameters.
;1) We don't want to miss hits to a model because our generative model has not covered a suitable range of coefficients 2) this dependence on coefficients can be utilised to constrain our search further.

; This problem can be alleviated with model classes.

;; Arity of Attributes


; Attributes
; TODO Definde declarative language here
; so features can take varying numbers of arguments, I either need to give a hard constraint
; that is, only use the correct number of features or do something more intelligent.
; How to do this? 
; Planning? 

; TODO - how to account for attributes with varying number of parameters?
; TODO - how will declarative-procedural component work?


(def monotonic-attr
  {:decl '(= y k) ;TODO, write in declarative form
   :proc (fn [a b] (apply + b))
   :vars '[a b]
   :name 'monotonic-attr})

(def nil-attr
  {:decl 'nil ;TODO, write in declarative form
   :proc (fn [a b] nil)
   :vars '[a b]
   :name 'nil-attr})

; (def smooth? 'is-smooth?)
; (def periodic? 'is-periodic)
; (def monotonic? 'monotonic?)

(def all-attrs
  [monotonic-attr
   nil-attr])

(defn eval-attr
  "Evaluate an attribute: Compute the real value of an attribute on some data."
  [attr subexprs data]
  ; (println "ATTR" attr "\nDATA" data "\nsubexprs" subexprs )
  (apply (:proc attr) (map #(data %) subexprs)))

; FIXME: UNFINISHED
(defn attr-val-compare
  "Compute the similarity between two attribute vectors"
  [attr-vals1 attr-vals2]
  1.0)

(defn sample-model-instance
  "Models have parameters which have distributions
   to be evaluated we sample these parameters"
   [model])

(defn gen-data
  "Generate data from a model.
  Currently works only with functional models, asuming lhs is dependent
  variable and rhs independent."
  [model]
  (let [n-model-samples 3
        n-points 10]
    (repeatedly n-model-samples #((:gen model) n-points))))

(defn eval-attr-perms
  "Evaluate the attributes for all permutations of the data

   Returns map, e.g. [{:subexprs [x y] attr-vals [1 2 3]}, ...]"
  [data attrs]
  ; for every perm. of vars, apply all the relevant attributes
  (for [subexprs (combo/permutations (keys data))]
    {:subexprs subexprs
     :attr-vals
        (map #(eval-attr % subexprs data)
             (filter #(= (count subexprs) (count (:vars %))) 
                      attrs))}))




















;; LEARNING


;TODO
; Define generative model
; Set of transformations

; Search algorith will take a a generative model which is a function
; Generate an initial solution by evaluating it
; It will then in a planning esque way apply a series of transformations
; Transformations have parameters
; These parameters wil come from a set of other functions

; What about errors, a evaluation may not work
; Where to store type data?
; 

; ; NOTEST
; (defn gen-f
;   "Generate initial set"
;   init-attrs)

; ; Transforms

; ; NOTEST
; (defn remove-elm
;   [coll name])

; ; NOTEST
; (defn add-elm
;   [coll])

; ; Analyses
; (defn num-elms
;   [coll]
;   (count coll))

; ; NOTEST
; ; Cost function
; (defn cost-f
;   "Cost yo"
;   )

; ; NOTEST
; (defn optimise
;   "The uber optimisation algorithm"
;   [gen-f transforms analyses cost-f]
;   )

; Dimensions of learning
; Error function -> classification based / information theoeretic based
; Generation vs finite list

; NOTEST

(defn gen-test-data
  "Generate Test data
  test-data should have form
  Model -> data
  e.g. sinx model -> {y [1 2 3] x [2 3 4]
    Meaning that there was a model
    But what about compound models
    ; Either we ask suggest-ext to return all 'good' models or we just give credit to
    any it gets right.
    So a better data structure is
    [sin-model power-model] -> {data}
    ")

; y = 2*sinx + z^2
; The arity of these models is different
; Both models are valid in the sense that y = sin x

(def test-data
  {[sin-model power-model] {'x [1 2 3] 'y [1 2 3]}})

(air)

(defn gen-weights-cost-f
  "Generate Cost function for set of attribute weights

   Cost func generates a test dataset which is a set of equations
   Evaluate the test dataset"

  [attrs]
  (let [data-data (somehow-gen-test-data)]
  
  (fn [params]
    (let [scores (map #(suggest-ext & models attrs model-attr-vals) test-data)]))))

; NOTEST
(defn learn-attr-weights
  "
  init-set is a f[]->list, which could just return a list
  or it could be a grammar"
  [attrs]
  (nelder-mead (ones (count attrs)) weights-cost-f))

(defn -main []
  (learn-attr-weights gen-f [remove-elm add-elm] [num-elms] cost-f))

; (defn -main []
;   (optimise gen-f [remove-elm add-elm] [num-elms] cost-f))