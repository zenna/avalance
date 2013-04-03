; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)
(use 'avalance.neldermead)


; TODO

; FIX EXPRESSION Generator
; * Avoid making nulls
; * How to evaluate models - nedler mead 

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
(defn create-compound-data
  "Converts data into compound data"
  [compounds data]
  ; For each model evaluate the best fit I can get of the data
  ; 1. Generate compound-data
  ; 2. (nelder-mead example-cost [5.0 5.0])
  (println compounds)
  (let [reformated-data (for [index (range (count (data 'a)))]
                              {'a (nth (data 'a) index) 'b (nth (data 'b) index)})
        new-data {'a (map (:as-lambda (first compounds)) reformated-data) 
                  'b (map (:as-lambda (second compounds)) reformated-data)}]
    ; (println "NEW DATA" new-data)
    new-data))
  

(defn eval-models
  "Returns error for best fit of all models against data"
  [compounds data models]
  ; For each model evaluate the best fit I can get of the data
  ; 1. Generate compound-data
  ; 2. (nelder-mead example-cost [5.0 5.0])
  (let [mm (map
    (fn [model]
      ; Construct new data from compounds
      ; Assume there will always be two compounds
      (let [cost-func (mean-sqr-error model data)
            best-param-fit-model (nelder-mead cost-func [1.0 1.0])]
        {:compounds compounds :model model :score (:cost best-param-fit-model)}))
      ; TODO: do least squares regression on model
    models)]
    (println mm "\n")
    mm))

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
            (> (count y-compounds) 2)
              (recur x-compounds (rest y-compounds) (concat weights (eval-models [x y] (create-compound-data [x y] data) models)))
            (> (count x-compounds) 2)
              (recur (rest x-compounds) (rest x-compounds) (concat weights (eval-models [x y] (create-compound-data [x y] data) models)))
            :else
              (concat weights (eval-models [x y] (create-compound-data [x y] data) models)))))))

; Models are expressions, which have parameters and variables
; Parameters are values to be optmised in eval-models
; Whereas variables come from compounds
(def exp-model
  {:as-lambda
  (fn [param-map indep-vars]
    (+ (param-map 'p1) (Math/pow (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :name 'exptmodel})

(def linear-model
  {:as-lambda
  (fn [param-map indep-vars]
    ; (println "MODEL!!" param-map indep-vars)
    (+ (param-map 'p1) (* (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :name 'linear})

(def models
  [exp-model
   linear-model])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn line
  [x]
  (+ 1.5 (* 10 x)))


(defn -main
  []
  ;1. Generate data 
  (let [data (gen-data-uniform line 1 100 10)
    ;2. Generate simple functions of data
    data-compounds (gen-data-compounds data 3)
    ;3. For each pair of compounds, produce plot, evaluate each plot on each fragment
    model-weights (find-bias data data-compounds models)]
    model-weights))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
(-main)