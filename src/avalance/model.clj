(ns ^{:author "Zenna Tavares"
      :doc "Initial set of models and transformations on models

            The induction algorithm finds equatiosn by extending
            existing models.

            A models describes a relationship between variables.
            Hence it has a declarative form (:as-expr), which is an expressions
            composed of real-valued variables and parameters.

            It also has a generative form (:gen), which samples
            values of its variables which adhere to the declarative contraint.
            Clearly there are many generative models for a declarative one."}
  avalance.model
  (:use avalance.relations)
  (:use avalance.suggest)
  (:use clozen.helpers))

; Models are expressions, which have parameters and variables
; Parameters are values to be optmised
; Whereas variables come from compounds of data
; (def exponent-model
;   {:as-expr '(= y (Math/pow pb x))
;   :params ['pb]
;   :vars ['y 'x]
;   :name 'exponent})

(def power-model
  {:as-expr '(= y (Math/pow x n))
  :dists {'n #(rand) 'x #(* 10 (rand))}
  :params ['n]
  :vars ['y 'x]
  :name 'power})

(def linear-model
  {:as-expr '(= y (+ c (* m x)))
   :dists {'c #(rand) 'm #(rand) 'x #(* 10 (rand))}
   :params '[c m]
   :vars '[y x]
   :name 'linear})

(def sin-model
  {:as-expr '(= y (Math/sin (* x p)))
   :dists {'p #(rand) 'x #(* 10 (rand))}
   :params ['p]
   :vars ['y 'x]
   :name 'sin})

(def constant-model
  {:as-expr '(= y k)
   :dists {'k #(* 10 (rand))}
   :params ['k]
   :vars ['y]
   :name 'constant})

(defn sample-vals
  "Return sampled vals sampled from distributions"
  [symbs dists]
  (zipmap symbs
          (map #((dists %)) symbs)))

(defn decl-to-gen
  "Convert a (functional) declarative model into a generative model
  Assumes model is of form y = f(x), not necessarily with those symbols,
  but that there is single term on lhs which does not appear on rhs."
  [model]
  (let [lhs (nth (:as-expr model) 1)
        rhs (nth (:as-expr model) 2)
        rhs-vars (filter #(in? (flatten rhs) %) (:vars model))        
        {as-lambda :as-lambda arg-map :arg-map}
          (make-model-lambda-rpl rhs (concat (:vars model) (:params model)))]

      (fn [n-points]
        (let [param-vals (sample-vals (:params model) (:dists model))]
          (apply merge-with cons-conj
            (repeatedly
              n-points
              #(let [var-vals (sample-vals rhs-vars (:dists model))]
                (merge  var-vals
                        {lhs
                          (apply as-lambda 
                          (place-args (merge param-vals var-vals)
                                      arg-map))}))))))))

(defn add-gen-model
  [model]
  (assoc model :gen (decl-to-gen model)))

(def models
  "Initial model set"
  (map add-gen-model
        [;constant-model
         ;exponent-model
         linear-model
         power-model
         sin-model]))

; Error functions - all binary
(def error-fs
  ['+ '*])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (Math/pow D (/ 3 2))))

(defn line
  [x]
  (+ 1.5 (* 10 x)))

(defn x-squared
  [x]
  (+ (* x x) (Math/sin x)))

(defn two-sin-x
  [x]
  (+ (* 2 (Math/sin x))))

(defn a-little-complex
  [x]
  (+ x  (* (Math/sin (* 2 x)) 3)))