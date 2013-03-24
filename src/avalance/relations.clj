; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)

; Returns two vectors or tuples for each datapoint
(defn gen-data-uniform
  "Uniformly generates real valued data from function"
  [func min-val max-val num-samples]
  (cond (< max-val min-val)
      (throw (Exception. "max-val must be greater than min-val"))
    :else
      (loop [x min-val xs [] fxs []]
        (cond (> x max-val)
          [xs fxs]
        :else
          (recur (+ x (/ (- max-val min-val) num-samples)) (concat xs [x]) (concat fxs [(func x)]))
        ))))

; TODO- must handle unary functions
(defn generate-data-compounds
  "Generate functions of data variables"
  [variables num-to-gen]
  (let [get-terminal #(rand-nth (concat variables [(rand 10)]))]
    (repeatedly num-to-gen #(random-code-custom-terminals 2 get-terminal))))

;new-x-data (map x-compound data) new-y-data (map y-compound data)
(defn compute-weights
  "This computes the weights"
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
        (cond
          (and (empty x-compounds) (empty y-compounds))
            weights
          (empty y-compounds)
            (recur (rest x-compounds) (rest x-compounds) (eval-models models))
          :else
            (recur x-compounds (rest y-compounds) (concat weights )))))))

; For each pair of compounds, I need to evaluate the likeliness of the model
(defn eval-models
  "Returns error for best fit of all models against data"
  [data models]
  ; For each model evaluate the best fit I can get of the data
  (map (fn [model]
    ; TODO: do least squares regression on model
    ()
    models)))

; What kind of data do I want out
; a/b vs b+a, matches closely with x^2
; Models
(* k x + c)
(expt k x)
(expt k)

; So the data I want out are best model and normalised
(* k x + c) (expt kx) best-mean-square

; I'm probably going to want to sort it.
; sort-by 
; [{:x-compound (a/b) :y-compound (b+1) [{model error}]}]

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn -main
  []
  ;1. Generate data 
  (let [data (gen-data-uniform planets 1 100 100)
    ;2. Generate simple functions of data
    data-compounds (generate-data-compounds '(a b) 10)
    ;3. For each pair of compounds, produce plot, evaluate each plot on each fragment
    model-weights (compute-model-weights data data-compounds models)
    ok (println data-compounds "\n")]
    
    
     (recur x-compounds (rest y-compounds) (concat weights ))))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
(-main)