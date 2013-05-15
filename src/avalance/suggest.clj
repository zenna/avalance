(ns avalance.suggest
  (:use avalance.relations))

; Attributes
; TODO Definde declarative language here
(def smooth? 'is-smooth?)
(def periodic? 'is-periodic)
(def monotonic? 'monotonic?)
(def attrs-procedures
  {monotonic (fn [data]
    )})
(def all-attrs
  [is-smooth is-periodic])

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
   [model]

; FIXME: This can replace extension
(defn suggest-model-extension
  "Suggest an extension to an equation"
  [model error-fs]
  (let [num-extensions (inc (rand-int 2))
        extension-vars (map #(symbol (str "e" %)) (range num-extensions))
        extended-expr
        ; Repeatedly reply a modification
        (loop [modified-equation equation extension-num 0]
          (if (= num-extensions extension-num)
            modified-equation
            ; Ignore first elements of list, which will be function symbols;
            ; We don't want to replace them as they don't evaluate by themselves to reals
            (let [all-keys (coll-to-keys equation (fn [elem pos] (or (zero? pos)
                                                                     (extension-var? elem))))
                  key-to-change (rand-nth (keys all-keys))
                  error-f (rand-nth error-fs)
                  value-at-key (all-keys key-to-change)
                  ext-name (nth extension-vars extension-num)
                  rand-replacement (rand-nth [(list error-f value-at-key ext-name)
                                             (list error-f ext-name value-at-key)])
                  new-equation (replace-in-sublist modified-equation key-to-change rand-replacement)]
              (recur new-equation (inc extension-num)))))]

    ; TODO: We need a better way to stop latter extensions overriding previous ones
    ; Hack for now: just check which extensions actually end up in the expression
    {:as-expr-ext extended-expr :ext-vars (filter #(in? (flatten extended-expr) %) extension-vars)}))

(defn eval-posterior
  "Evaluate the posterior of 'model in expression'"
  [attr-vals model subexprs-data var-bindings]
  (let [num-gen 100
        ; Generate
        sim-vals (for [i (range num-gen)]
                      :let [ext-model (suggest-extension model error-fs)]
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

