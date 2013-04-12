; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo]))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)
(use 'avalance.neldermead)
(use 'avalance.helpers)



; TODO
; Handle nil!
; Return right data structure from gen thing
; Implement expressiong testing to prevent stupid expressions
; bind variables to model
; update cost func to take into account new model form
; decide accept criterion
; write code to
; choose sub expression of 

; If I input into the functions a map:
; Then 1. I waste computation dereferences it
; It's harder to mess up

; If I make the functions of the arguments they are supposed to be
; Then lets say my data is x y and I generate x + y/ y
; I need to make sure the order is correct
; This happens in transform data.
; It means I need to know for a particular function, which position a particular symbol is bound to
; This means attaching more more information to the procedur
; OR generating them in a consistent way
; Could attach to it {x 0 y 1}

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

(defn rearrange-args
  ""
  [datapoint arg-map]
  (map #(datapoint (first %1)) arg-map))

(defn transform-data
  "Applies subexpr to data to generated transformed dataset"
  [expr data]
  (let [data-vars (keys data)
        data-length (count (data (first (keys data))))]
    ; Extract corresponding datapoints, e.g. a1 b1 c1 and transform through compound
    (vec (for [index (range data-length)
          :let [datapoint (zipmap data-vars
                                  (map #(nth (data %) index) data-vars))]]

      ; here ill have a:0 b:4  and function a:1 b:0
      (apply (:as-lambda expr) (rearrange-args datapoint (:arg-map expr)))))))

(defn build-expr-metadata
  [expr data-vars]
  "Convert a subexpression to one with executable lambda and argument map"
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
    (vec (for [symb data-vars
          :when (in? flat-expr symb)]
      symb))]
    ; (println "EXPR" expr  "VARS" vars-in-expr "DATAVARS" data-vars "FLAT "flat-expr "LISt?" (symbol? expr) )
    ; We need to find out which data variables are actually in the expression
    {:as-expr expr
     :as-lambda (make-lambda-args expr vars-in-expr)
     ; arg-map is maps data variable to position in argument list e.g. {'a 0 'b 1}
     ; it must be sorted for convenience in rearrange-args 
     :arg-map (sort-by val < (zipmap vars-in-expr (range (count vars-in-expr))))
     :vars vars-in-expr}))


; TODO make this return the data too and do the hard generating part
(defn gen-subexprs
  "Generate functions of data variables"
  [data num-to-gen]

  ; TODO I AM USING ALL THE DATA VARS AS THE INPUT TO THE FUNCTIONS, E.G (defn myfn [x y z] (+ x y))
  ;      SHOULD SEE WHICH VARS ARE ACTUALLY USED IN EXPRESSION
  (let [data-vars (vec (keys data))
        data-prods (map (fn [variable] {:prod variable :weight 1.0}) data-vars)
        new-pcfg (add-data-to-pcfg data-prods compound-pcfg)
        expr-exprs-data
          (loop [exprs {} num-left-to-gen num-to-gen]
            (let [expr (gen-expr-pcfg new-pcfg)
                  expr-exec (build-expr-metadata expr data-vars)
                  expr-data {expr (transform-data expr-exec data)}]
              (cond
                (zero? num-left-to-gen)
                exprs

                (and (expr-covaries? expr-data data) (expr-unique? expr-data exprs data))
                (recur (merge exprs expr-data) (dec num-left-to-gen))

                :else
                (recur exprs num-left-to-gen))))]
    
    ; First generate expressions now package it up nicely
    {:subexprs

     ; PERFORMANCE: MEMOIZE build-expr-metadata
     (map #(build-expr-metadata %1 data-vars) (keys expr-exprs-data))

     :subexprs-data
     expr-exprs-data}))

; a model is an expression :expr (+ (* 'm x) 'c) :params ['m 'c'] :independent-vars
; data {'a [1 2 3] 'b [1 2 3]}
; assumes data is same size
; assumes data is a and b
; as
; need to go from [1 2 3 4] and ['m 'c 'a 'b'] to {'m: 1.2}
; TODO GENERALISE THIS TO F, maybe, and use for declaring variables

(defn mean-sqr-error
  "Take a model and a dataset and produce a function which when given a set of parameters of the model
  will compute the mean squared error of the model against data"
  [model data var-binding]
  ; var-binding e.g. {a: x b: y}
  ; model: lhs-arg-map e.g. {'x 0 'c 1 'm 2}
  ; so for every symbol in model arg-map
  ; I need to take symbol determine whether it is a variable or a parameter
  ; if it is a variable I need to look it up in var-binding
  ; e.g. x-> variable -> a and put it in the vector position at that slot 0
  ; variable binding is model-variabl -> datavariable e.g. x->a

  ; PERFORMANCE I COULD USE PARTIAL EVALUATION TO MAKE THIS FASTER I.E DO THE BINDING ONCE
  (fn [param-values]
    ; WARNING CODE ASSUMES args is sorted
    ; (println "model" model "\n\n Data" data "\n\n varbinding" var-binding)
    (let [param-binding (zipmap (:params model) param-values)
          data-size (count (data (first (keys data))))]
          ; (println "param-binding" param-binding "\n\n datasize" (:lhs-arg-map model))

      (loop [accum-error 0.0 index 0]
        (if (>= index data-size)
          (/ accum-error 2)
          (let [lhs-rhs
            (map
              (fn [model-side]
                ; (println "mdoel-side" model-side )
                (let [args (for [arg (keys (:arg-map model-side))
                          :let [value (if (in? (:vars model) arg)
                                          (nth (data (var-binding arg)) index)
                                          (param-binding arg))]]
                          value)]
                    ; (println "args!!!!" args "keys!!!" (keys (:arg-map model-side) ))
                  (apply (:as-lambda model-side) args)))
              
              [{:arg-map (:lhs-arg-map model) :as-lambda (:lhs-as-lambda model)}
               {:arg-map (:rhs-arg-map model) :as-lambda (:rhs-as-lambda model)}])

                 error (- (first lhs-rhs) (second lhs-rhs))]

            (recur (+ accum-error (Math/pow error 2))
                   (inc index))))))))

; Returns [what is love! baby dont hurt me]
(defn eval-model
  "Returns error for best fit of all models against data"
  [subexprs data model var-binding]
    (let [cost-func (mean-sqr-error model data var-binding)
          best-param-fit-model (nelder-mead cost-func [1.0 1.0])]
      {:subexprs subexprs :model model :score (:cost best-param-fit-model)}))

; Selection of models should be based on the data, and the available models
(defn sample-model
  "choose models"
  [sampled-models models data])

(defn continue?
  "Should we continue down this path"
  [score model num-tests-left])

(defn accept-model?
  "Should we continue down this path"
  [score model num-tests-left])

(defn bind-data-to-model
  "Creates a mapping between the variables of a sub expression and those of model"
  [data model]
  ; (println "DATA" data "MODEL" model)
  (zipmap (:vars model) (keys data)))

; The output of this function should be a list of size >= 1 of models with parameters instantiated

; There are three loops
; Should I should I try a new set of subexprs?
  ; - No? return my equations
; For this given set of subexprs should I try a new model
  ; - No? Return
; For this model should I try a new error extension?

; Terms: subexpr - a function of the data, which when evaluated will return a real number
        ; model   - a predicate expression containing parameters as well as variables, cannot be evaluated unless params are instantiated
        ; equation - a (set of) models with params instatiated

(defn find-expr
  "Searches for an expression"
  [data models]
  (let [num-subexprs 2;(inc (rand-int 2)); TODO this should be dependent on the number of variables in the data
        max-num-plots 20]
    (loop [equations [] num-plots-left max-num-plots]
      (let [subexprs-subexprs-data (gen-subexprs data num-subexprs)
            subexprs (:subexprs subexprs-subexprs-data)
            subexprs-data (:subexprs-data subexprs-subexprs-data)
            equations (conj equations
        (loop [sampled-models []
               equations []]
          (let [;model (sample-model sampled-models models data)
                model (first models)
                sampled-models (conj sampled-models model)
                var-binding (bind-data-to-model subexprs-data model)
                score (eval-model subexprs subexprs-data model var-binding)]
                (println score)
            ; I've now chosen a model, I need to decide if a) I need to fit this model b)
            ; What's wrong is that if the score is perfect then I need go no further
            ; Also I may want a memory of what I have tried so far, I don't want to forget
            ; I also 
            ; The test should tell us whether to a) take one of the sampled models, b) to sample some more models
            ; or c) to try to extend a model
            (cond
              true;(accept-equation? equation scores num-tests-left)
              score

              ; (extend?)
              ; (extend the model the model)
              ; ; If I can't find a good extension then I might one to try a new model, or a I might want to just accept this model
              ; ; 

              :else
              (recur sampled-models equations)))))]
        
        (cond
          false
          ; (try-more-subexprs? equations)
          (recur equations (dec num-plots-left))

          :else
            equations)))))


; Models are expressions, which have parameters and variables
; Parameters are values to be optmised in eval-models
; Whereas variables come from compounds

; Am I saying a model is a function relating a dependent variable to independent variables
; What if I have a single x
; A more general representation of a model is a generative model
; - A program that generates the data
; I need a representation of a model, that allows both things lioke cos(a+b) but also dependent indepoedent relationships
; and is comptuable

(def exp-model
  {:as-lambda
  (fn [param-map indep-vars]
    (+ (param-map 'p1) (Math/pow (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :vars ['x]
  :name 'exptmodel})

(def power-model
  {:as-lambda
  (fn [param-map indep-vars]
    (+ (param-map 'p1) (Math/pow (indep-vars 'x) (param-map 'p2))))

  :params ['p1 'p2]
  :vars ['x]
  :name 'exptmodel2})

; (def linear-model
;   {:as-lambda
;   (fn [param-map indep-vars]
;     ; (println "MODEL!!" param-map indep-vars)
;     (+ (param-map 'p1) (* (param-map 'p2) (indep-vars 'x))))

;   :params ['p1 'p2]
;   :vars ['x]
;   :name 'linear})

(def linear-model
  {:as-expr '(= y (+ c (* m x)))
   :rhs-as-lambda
   (fn [x c m]
     (+ c (* m x)))
   :rhs-arg-map {'x 0 'c 1 'm 2}
   :lhs-as-lambda
   (fn [y]
    ; (println "MODEL!!" param-map indep-vars)
     y)
   :lhs-arg-map {'y 0}

   :params ['c 'm]
   :vars ['y 'x]
   :name 'linear})

; TODO ax^2 + bx + c
(def quadratic-model
  {:as-lambda
  (fn [param-map indep-vars]
    ; (println "MODEL!!" param-map indep-vars)
    (+ (param-map 'p1) (* (param-map 'p2) (indep-vars 'x))))

  :params ['p1 'p2]
  :vars ['x]
  :name 'linear})

; (def models
;   [exp-model
;    power-model
;    linear-model
;    ])

(def models
  [linear-model])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn line
  [x]
  (+ 1.5 (* 10 x)))

(defn a-little-complex
  [x]
  (+ (* x x) (* (Math/sin x) 3)))

(def dt {'a [1 2 3] 'b [10 14 12]})
; (def subsa (gen-subexprs dt 2))
; (def okb (:subexprs-data subsa))
; (println "C" (bind-data-to-model okb linear-model))

; ; (println "oakdaok" (first okb));(concat (:vars (first okb))  (:vars (second okb))))

; (println "OK" (reduce #(concat (:vars %1) (:vars %2)) okb))

(find-expr dt models) 

(defn -main[])

; (defn -main
;   []
;   ;1. Generate data 
;   (let [data (gen-data-uniform a-little-complex 1 100 10)]
;         ; model-weights (subvec (vec (sort-by :score (find-expr data models))) 0 5)]
;     data))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
; (-main)