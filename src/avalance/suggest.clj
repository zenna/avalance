(ns ^{:doc "Suggest extensison to some expr."
      :author "Zenna Tavares"}
avalance.suggest
  (:use avalance.grammar)
  (:require [clojure.math.combinatorics :as combo]))

(defn match-selector
  "Selects a match.
  
  Mutiple patterns may match, and for a given pattern,
  a variable may match multiple times.  We need to select one.
  
  Can not select more than one since replacement might invalidate
  a matched pattern"
  [matches]
  ; two rand-nth, first to select the pattern, then to select the match
  ; for a pattern
  {:ptrn 'ALL :match (rand-nth (rand-nth matches))})

(def prob-mod-gram
  "A probabilstic transformation grammar"
  {:selector match-selector
   :pattern-evaluator pattern-evaluator
   :pattern-rules
   {'ALL [{:prod '(UF ?match) :weight 1.0}
          {:prod '(BF V ?match) :weight 1.0}
          {:prod '(BF ?match V) :weight 1.0}]}
    :rules
    {'UF [{:prod 'Math/sin :weight 1.2}
          {:prod 'Math/cos :weight 1.2}]
     'BF [{:prod '+   :weight 1.0}
          {:prod '-   :weight 1.0}
          {:prod '*   :weight 1.0}
          {:prod '/   :weight 1.0}]}})

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
   :proc (fn [a b] true)
   :vars '[a b]
   :name 'monotonic-attr})

(def smooth? 'is-smooth?)
(def periodic? 'is-periodic)
(def monotonic? 'monotonic?)
(def attrs-funcs
  {monotonic? (fn [a b]
    true)})
(def all-attrs
  [monotonic-attr])

(defn eval-attr
  "Evaluate an attribute"
  [attr subexprs data]
  (println "ATTR" attr "\nDATA" data "\nsubexprs" subexprs )
  (apply (:proc attr) (map #(data %) subexprs)))

(defn attr-val-compare
  "Compute the similarity between two attribute vectors"
  [attr-vals1 attr-vals2])

(defn sample-model-instance
  "Models have parameters which have distributions
   to be evaluated we sample these parameters"
   [model])

(defn gen-data
  "Generate data from a model.
  Currently works only with functional models, asuming lhs is dependent
  variable and rhs independent."
  [model]
  (let [n-model-samples 2]
  (println "ok let's try" model)))

(defn eval-posterior
  "Evaluate the posterior of 'model in expression'"
  [attr-vals model subexprs-data var-bindings])

(defn eval-attr-perms
  "Evaluate the attributes for all permutations of the data"
  [data attrs]
  (for [subexprs (combo/permutations (keys data))]
                         (map #(eval-attr % subexprs data)
                               (filter #(= (count subexprs) (count (:vars %))) 
                                        attrs))))
;TODO - incomplete
(defn extend-model
  "extends a model using a pmg"
  [model]
  model)

(defn find-attr-vals
  "Compute attribute values for a model."
  [model attrs]
  (let [n-ext-gen 0
        cluster (concat [model] (repeatedly n-ext-gen (extend-model model)))
        cluster-data (map #(gen-data %) cluster)]
    {:focal-model model
     :cluster-attr-vals (map eval-attr-perms cluster-data attrs)}))

; Entry Point
(defn suggest-ext
  [data models attrs]
  "Given some data it will suggest a (number of?) models and extensions.
  Suggest model and extension"
        ; For each permutation of exprs, evaluate attributes
  (let [attr-vals (eval-attr-perms data attrs)]
        ; posteriors (map #(eval-posterior attr-vals % attrs subexprs-data
        ;                                  var-bindings) 
        ;                 models)]
    attr-vals))
    ; (rand-nth-reciprocal-categorical models posteriors)))

(defn find-counter-factuals
  [attr-vals]
  "Search through space of values of attrs (i.e. propose counter-factuals)
   and look for those that increase posterior probability.
   Returns attr-vals"
  )