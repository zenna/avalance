(ns ^{:doc "Suggest extensison to some expr."
      :author "Zenna Tavares"}
avalance.suggest
  (:use avalance.grammar))

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
(def smooth? 'is-smooth?)
(def periodic? 'is-periodic)
(def monotonic? 'monotonic?)
(def attrs-procedures
  {monotonic? (fn [data]
    true)})
(def all-attrs
  [smooth? periodic?])

(defn eval-attr
  "Evaluate an attribute"
  [attr data]
  ((attrs-procedures attr) data))

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
    

(defn eval-posterior
  "Evaluate the posterior of 'model in expression'"
  [attr-vals model subexprs-data var-bindings]
  (let [num-gen 100
        ; Generate
        sim-vals (for [i (range num-gen)]
                      :let [ext-model (extend-expr (:as-expr model))]
                      (attr-val-compare attr-vals (eval-attr (gen-data ext-model) data)))]
    (mean sim-vals)))
  ; Generate N models where that is true wowho ho hoo
  ; have some similarity metric on the attr-vals

; Entry Point
(defn suggest-model-and-extension
  [data models attrs subexprs-data var-bindings]
  "Given some data it will suggest a (number of?) models and extensions.
  Suggest model and extension"
  ;1 Choose a model to extend
  (let [attr-vals (map #(eval-attr % data) attrs)
        posteriors (map #(eval-posterior attr-vals % attrs subexprs-data var-bindings) models)]
    (rand-nth-categorical models posteriors)))

(defn find-counter-factuals
  [attr-vals]
  "Search through space of values of attrs (i.e. propose counter-factuals)
   and look for those that increase posterior probability.
   Returns attr-vals"
  )