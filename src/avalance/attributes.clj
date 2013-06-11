(ns ^{:doc "Attribute (feature) selection and learning
            There are three dimensiosn over solutions which we seek to explore,
            1. The criterion for a good feature
            2. The method for generating new features
            3. The search algorithm"
      :author "Zenna Tavares"}
avalance.attributes
  (:use avalance.grammar)
  (:use clozen.helpers)
  (:require [clojure.math.combinatorics :as combo]))

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