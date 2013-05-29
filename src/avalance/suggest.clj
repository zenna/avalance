(ns ^{:doc "Suggest extensison to some expr."
      :author "Zenna Tavares"}
avalance.suggest
  (:use avalance.grammar)
  (:use clozen.helpers)
  (:require [clojure.math.combinatorics :as combo]))

; Feature learning
; Abstract out learning algorithm, try first classification error/but leave room for some information theoretic 

; The values of the coefficients are important; they may affect whether there is a single mode, whether it is sloping downwards and so on.
; This has implications for our feature learning, since 1) We don't want to miss hits to a model because our generative model has not covered a suitable range of coefficients 2) this dependence on coefficients can be utilised to constrain our search further.

;However there is something certianly missing.  I use features of the data to infer constraints on the coefficients in a non-generative manner.  For instance if it passes through the origin

; There are many intuitions we would like to capture, perhaps it is best to list examples

; Noise!
; Noiseless

; TODO;
; 2. Implement many generative models and generative model generator
; 3. Incorporate n-ary argument attributes and models, mixture models and the like

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
  "Evaluate an attribute"
  [attr subexprs data]
  ; (println "ATTR" attr "\nDATA" data "\nsubexprs" subexprs )
  (apply (:proc attr) (map #(data %) subexprs)))

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
;TODO - incomplete
(defn extend-model
  "extends a model using a pmg"
  [model]
  (assoc model :name 'extended-model))

(defn find-attr-vals
  "Compute attribute values for a model."
  [model attrs]
  (let [n-ext-gen 1
        cluster (concat [model] (repeatedly n-ext-gen #(extend-model model)))
        cluster-data (apply concat (map #(gen-data %) cluster))]
    {:focal-model model
     :cluster-attr-vals (apply concat 
                               (map #(assoc-coll (eval-attr-perms %1 attrs)
                                                 :model %2)
                                cluster-data cluster))}))

(defn eval-posterior
  "Evaluate the posterior of 'model in expression'"
  [attr-vals model-attr-vals]
  (println "we made it")
  (let [likelis (for [data-attr attr-vals
                     model-attr model-attr-vals]
                 {:data-attr data-attr :model-attr model-attr
                  :likelihood
                 (map #(attr-val-compare data-attr %)
                      (:cluster-attr-vals model-attr))})
        scores (map mean (extract likelis :likelihood))
        {data-attr :data-attr model-attr :model-attr likelihood :likelihood}
          (rand-nth-reciprocal-categorical likelis scores)]
    (rand-nth-reciprocal-categorical
      (:cluster-attr-vals model-attr) likelihood)))

; Entry Point
(defn suggest-ext
  [data models attrs model-attr-vals]
  "Given some data it will suggest a (number of?) models and extensions.
  Suggest model and extension"
        ; For each permutation of exprs, evaluate attributes
  (let [attr-vals (eval-attr-perms data attrs)
        posteriors (eval-posterior attr-vals model-attr-vals)]
    posteriors))
    ; (rand-nth-reciprocal-categorical models posteriors)))

(defn find-counter-factuals
  [attr-vals]
  "Search through space of values of attrs (i.e. propose counter-factuals)
   and look for those that increase posterior probability.
   Returns attr-vals"
  )