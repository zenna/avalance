; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)
(use 'avalance.neldermead)


; TODO
; Handle nil!

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

(defn expr-covaries?
  [expr data]
  "Does the exprs covary with the data"
  true)

(defn expr-unique?
  [expr exprs data]
  "Is this exprs not already in the list of exprs"
  true)

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
      (loop [exprs [] num-left-to-gen num-to-gen]
        (let [expr (gen-expr-pcfg new-pcfg)
              expr-data (create-compound-data compounds data)]
          (cond
            (zero? num-left-to-gen) exprs

            (and (expr-covaries? expr data) (expr-unique? expr exprs data))
              (recur (conj exprs expr) (dec num-left-to-gen))

            :else
              (recur exprs num-left-to-gen)))))))

(defn create-compound-data
  "Applies compounds to data to generated transformed dataset
  Returns: ['a []"
  [compounds data]
        ; Convert from {'a [1 2 3] 'b [4 5 6]} -> [{'a 1 'b 3} {'a 2 'b 4} ..]
  (let [reformated-data (for [index (range (count (data 'a)))]
                              {'a (nth (data 'a) index) 'b (nth (data 'b) index)})
        new-data {'a (map (:as-lambda (first compounds)) reformated-data) 
                  'b (map (:as-lambda (second compounds)) reformated-data)}]
    ; (println "NEW DATA" new-data)
    new-data))
  
; Returns [what is love! baby dont hurt me]
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
    ; (println mm "\n")
    mm))

(defn find-expr
  "Searches for an expression"
  [data models]
  (let [num-compounds 2;(inc (rand-int 2)); TODO this should be dependent on the number of variables in the data
        num-tests 20]
    (loop [weights [] num-tests-left num-tests]
      (let [compounds (gen-data-compounds data num-compounds)]
        (cond
          (zero? num-tests-left)
            weights

          :else
            (recur (conj weights (eval-models compounds (create-compound-data compounds data) models)) (dec num-tests-left)))))))

; Models are expressions, which have parameters and variables
; Parameters are values to be optmised in eval-models
; Whereas variables come from compounds
(def exp-model
  {:as-lambda
  (fn [param-map indep-vars]
    (+ (param-map 'p1) (Math/pow (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :name 'exptmodel})

(def power-model
  {:as-lambda
  (fn [param-map indep-vars]
    (+ (param-map 'p1) (Math/pow (indep-vars 'x) (param-map 'p2))))

  :params ['p1 'p2]
  :name 'exptmodel2}) 

(def linear-model
  {:as-lambda
  (fn [param-map indep-vars]
    ; (println "MODEL!!" param-map indep-vars)
    (+ (param-map 'p1) (* (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :name 'linear})

(def models
  [exp-model
   power-model
   linear-model
   ])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn line
  [x]
  (+ 1.5 (* 10 x)))

(defn a-little-complex
  [x]
  (+ (* x x) (* Math/sin x) 3))

(defn -main
  []
  ;1. Generate data 
  (let [data (gen-data-uniform a-little-complex 1 100 10)
        model-weights (subvec (vec (sort-by :score (find-expr data models))) 0 5)]
    model-weights))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
(-main)