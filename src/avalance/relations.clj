; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)

; TODO

; FIX EXPRESSION Generator
; * Avoid making nulls
; * Making a probabilsitic grammar where I can change (i.e search over the weights)
; * Create nary functions - maybe simple type system

; * How to evaluate models - Bayesian Linear regressin 
; * 

; Returns two vectors or tuples for each datapoint
(defn gen-data-uniform
  "Uniformly generates real valued data from function"
  [func min-val max-val num-samples]
  (cond (< max-val min-val)
      (throw (Exception. "max-val must be greater than min-val"))
    :else
      (loop [x min-val xs [] fxs []]
        (cond (> x max-val)
          {'a xs 'b fxs}
        :else
          (recur (+ x (/ (- max-val min-val) num-samples)) (concat xs [x]) (concat fxs [(func x)]))
        ))))

(defn gen-data-compounds
  "Generate functions of data variables"
  [data num-to-gen]

  ; Create terminals
  ; For convenience, functions are unary, expecting "data" arg
  (let [data-prods (map (fn [variable] {:prod (list 'data (list 'quote variable)) :weight 1.0}) (keys data))
      new-pcfg (add-data-to-pcfg data-prods compound-pcfg)
      expr-depth 2]
    (map
      ; Convert expr -> {:as-expr expr :as-lambda computeable-expr}
      (fn [expr] {:as-expr expr :as-lambda (make-lambda-args expr '[data])})
      (repeatedly num-to-gen #(gen-expr-pcfg new-pcfg)))))

; For each pair of compounds, I need to evaluate the likeliness of the model
; Returns [{:compounds [list of compounds] :model AMODEL :score 123 }, ...]
(defn eval-models
  "Returns error for best fit of all models against data"
  [compounds data models]
  ; For each model evaluate the best fit I can get of the data
  (map
    (fn [model] {:compounds compounds :model model :score 123})
      ; TODO: do least squares regression on model
    models))

; Data is a matrix, vector of vectors: one for each variable over some interval.
; Returns [{:compounds [list of compounds] :model AMODEL :score 123 }, ...]
(defn find-bias
  "Finds the biases for search: for the data and all pairs of compounds, we scores the models"
  [data data-compounds models]

  ; Sanity check for size of compounds
  (cond
    (<= (count data-compounds) 2)
      (throw (Exception. "Size must be gte to 2 since all pairs are different"))
    :else
      ; Data-compounds are  simple functions of data variables, e.g. a/b or sin(a)
      ; We want to check all different pairs of compounds against our models
      (loop [x-compounds data-compounds y-compounds data-compounds weights []]
        (let [x (first x-compounds) y (first (rest y-compounds))]
          ; (println (count x-compounds) "-" (count y-compounds) "->" (:as-expr x) "--- VS ---" (:as-expr y))
          (cond
            (and (empty? x-compounds) (empty? y-compounds))
              weights
            (empty? y-compounds)
              (recur (rest x-compounds) (rest x-compounds) (concat weights (eval-models [x y] data models)))
            :else
              (recur x-compounds (rest y-compounds) (concat weights (eval-models [x y] data models))))))))

; Models are expressions, which have parameters and variables
; Parameters are values to be optmised in eval-models
; Whereas variables come from compounds
(def models
  [ '(+ (* p1 x) p2)
    '(expt p1 x)
    '(sin x)])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn -main
  []
  ;1. Generate data 
  (let [data (gen-data-uniform kepler 1 100 100)
    ;2. Generate simple functions of data
    data-compounds (gen-data-compounds data 10)
    ;3. For each pair of compounds, produce plot, evaluate each plot on each fragment
    model-weights (find-bias data data-compounds models)]
    model-weights))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
(-main)