(ns avalance.suggest)

; Attributes
(def is-smooth '())
(def is-periodic '())

(def attrs
  {is-smooth (fn [data] smooth?)})

(defn eval-attr
  "Evaluate an attribute"
  [attr data]
  ((procedures attr) data))

(def all-attrs
  [is-smooth is-periodic])

(defn suggest-model-and-extension
  [data]
  "Suggest model and extension"
  ;1 Choose a model to extend
  (let [what (map (fn [model])
                  models)])
  ;2 evaluate all bangers
  ;3

(defn eval-posterior
  "Evaluate the posterior of 'model in expression'"
  [attr-vals])

(defn what
  [model]
  (let [attr-vals (map #(eval-attr % data) all-attrs)
        posterior (eval-posterior attr-vals)
        find-counter-facts])