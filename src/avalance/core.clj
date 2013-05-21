(ns ^{:doc "Symbolic Regression main."
      :author "Zenna Tavares"}
  avalance.core
  (:use avalance.relations)
  (:use avalance.suggest)
  (:use clozen.helpers))

; Models are expressions, which have parameters and variables
; Parameters are values to be optmised
; Whereas variables come from compounds of data
(def exponent-model
  {:as-expr '(= y (Math/pow pb x))
  :params ['pb]
  :vars ['y 'x]
  :name 'exponent})

(def power-model
  {:as-expr '(= y (Math/pow x n))
  :params ['n]
  :vars ['y 'x]
  :name 'power})

(def linear-model
  {:as-expr '(= y (+ c (* m x)))
   :params ['c 'm]
   :vars ['y 'x]
   :name 'linear})

(def sin-model
  {:as-expr '(= y (Math/sin (* x p)))
   :dists {'p #(rand) 'x #(* 10 (rand))}
   :params ['p]
   :vars ['y 'x]
   :name 'sin})

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

(def constant-model
  {:as-expr '(= y k)
   :param-sampler {'k #(* 100 rand)}
   :gen (fn [n-points] ())
   :params ['k]
   :vars ['y]
   :name 'constant})

(def models
  "Initial model set"
  (map add-gen-model
        [;constant-model
         ;exponent-model
         ;linear-model
         ;power-model
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

(def data (gen-data-uniform line 10 100.0 5))
(println "Data Is" data)

;sols (find-expr data models error-fs 0 [])]

(defn -main
  []
  (let [sols (new-find-expr data models [] all-attrs)]
    (println "\n\nThe unformatted solutions are:" sols)
    (println "\n\nThe formatted solutions are:" (map compile-expr sols))))