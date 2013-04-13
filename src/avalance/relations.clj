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


(defn gen-subexprs
  "Generate functions of data variables"
  [data num-to-gen]

  (let [data-vars (vec (keys data))
        data-prods (map (fn [variable] {:prod variable :weight 10.0}) data-vars)
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


(defn make-model-lambda2
  "Make evaluable model rhs/lhs"
  [expr vars params]
  ; 1. find out which of the vars or params are in the subexpr
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
          (vec (for [symb (concat vars params)
                :when (in? flat-expr symb)]
            symb))
        arg-map (zipmap vars-in-expr (range (count vars-in-expr)))]
  ; (println "EXPR" expr "\nvars!" vars "\nparams" params "\nargmap" (sort-by val < arg-map)) 
  {:as-lambda  (make-lambda-args expr vars-in-expr) :arg-map (sort-by val < arg-map)}))

(def make-model-lambda (memoize make-model-lambda2))


(defn sum-sqr-error
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
          data-size (count (data (first (keys data))))
          {expr :as-expr vars :vars params :params} model
          ; ok (println "EXPRARARA" expr)
          lhs (nth expr 1)
          rhs (nth expr 2)]
          ; (println "param-binding" param-binding "\n\n datasize" (:lhs-arg-map model))

      (loop [accum-error 0.0 index 0]
        (if (>= index data-size)
          (/ accum-error 2)
          (let [lhs-rhs
            (map
              (fn [model-side]
                ; (println "mdoel-side" model-side "WHAT?\n" (:arg-map model-side) "\n")
                ; args = list of arguments for side of equation
                ; found by 
                (let [args (for [arg (keys (:arg-map model-side))
                          :let [value (if (in? (:vars model) arg)
                                          (nth (data (var-binding arg)) index)
                                          (param-binding arg))]]
                          value)]
                    ; (println "args!!!!" args "keys!!!" (keys (:arg-map model-side) ))
                  ; (println (if (nil? (first args)) ["a" args "\nms" model-side "\npb" param-binding "\nds" data-size "\ndata" data "\nvb" var-binding] '() ))
                  (apply (:as-lambda model-side) args)))
              
              [(make-model-lambda lhs vars params)
               (make-model-lambda rhs vars params)])

                 error (- (first lhs-rhs) (second lhs-rhs))]

            (recur (+ accum-error (Math/pow error 2))
                   (inc index))))))))

(defn compile-equation
  "Create an equation from a model and some ")

; Returns [what is love! baby dont hurt me]
(defn eval-model
  "Returns error for best fit of all models against data"
  [subexprs data model var-binding]
    (let [cost-func (sum-sqr-error model data var-binding)
          best-fit-params (nelder-mead cost-func [1.0 1.0])]
      {:subexprs subexprs :model model :score (:cost best-fit-params)}))


; Returns {'v1 [1 2 3] 'v2 [3 4 5]}
(defn fit-extensions
  "Takes an equation with extensions and returns a new dataset for each extension"
  [equation])
  ; (def extension {:as-expr (= y (+ (sin (+ v x)) v2)) :params ['v1 'v2] :vars ['y 'x]
  ; Now the difference is that I am not trying to fit one v but create a new dataset

  ; 1. I need to create a new rhs-lambda lhs-lambda
  ; 2. I could just make a function which makes the rhs-lhs lambda from a model
  ; 3. But first update the model by insantating the parameters and adding in the vs
  ; 4. Then I need to abstract out details of mean-sqr-error and make another function which
  ; 

; Selection of models should be based on the data, and the available models
(defn sample-model
  "choose models"
  [sampled-models models data])

(defn continue?
  "Should we continue down this path"
  [score model num-tests-left])

; (defn accept-equation?
;   "Should we continue down this path"
;   ;. If no more tests lef then accept of course
;   ;. Otherwise we need to compute posterior
;   ;. Accepting means I will use this 
;   [score model num-tests-left]
;   (cond (zero num-tests-left?) false
;     ))
  ; 1. How to represent equation
  ; 2. How to compute prior, complexity of the equation, or whole equation
  ; 3. How to choose

(defn bind-data-to-model
  "Creates a mapping between the variables of a sub expression and those of model"
  [data model]
  {:pre [(= (count (keys data)) (count (:vars model)))]}
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
        ; equation - a (set of) models with params instatiated to values and variables to (functions of) data variables

; e.g. if I will suggest an extension to the model sinx, i'll use the knowledge that y=sinx
; I need to know what I am extending
; and the model!
; What will I return? well I will
; return a new expression, modified with some new term
; e.g. y = sin x to y = sin (x + v), and some indication v is special
; 1. Should this be in the model selection stage? There may be multiple extensions per model
; 2. Should I be modifying models or equations?

; if I extend a model e.g . y = psin(px)
; to find v i need to instatiate the parameters
; so it should be an equation
(defn suggest-extension
  "Suggest an extension to an equation"
  [equation data]
  (let [num-extensions (rand-int 2)
        ])
  ; select a random branch
  ; select a random error function
  ; select a random positon 1 or zero
  ; return
  )

(defn find-expr
  "Searches for an expression"
  [data models]
  (let [num-subexprs 2;(inc (rand-int 2)); TODO this should be dependent on the number of variables in the data
        max-num-plots 20]

    ; loop over (samples of) subexpression sets
    (loop [equations [] num-plots-left max-num-plots]
      (let [subexprs-subexprs-data (gen-subexprs data num-subexprs)
            subexprs (:subexprs subexprs-subexprs-data)
            subexprs-data (:subexprs-data subexprs-subexprs-data)
            what (println "DATA-VARS" (keys subexprs-data))
            equations (conj equations
        
        ; Loop through different models
        (loop [sampled-models []
               equations []]
          (let [model (first models)
                sampled-models (conj sampled-models model)
                var-binding (bind-data-to-model subexprs-data model)
                equation (eval-model subexprs subexprs-data model var-binding)]
                (println equation)
            
            ; Should I extend the model or not?
            (cond

              ; (accept-equation? equations scores num-tests-left)
              true
              equation

              ; The problem with using an equation from eval-model is that I may want to keep my model params
              ; Why?
              ; Because i may find an extension which makes it better e.g real equation y = 2.3sin(root x) + 2
              ; my model is y = sin(x)
              ; I propose y = sin(v + x)
              ; I might to keep the parameters around, just in case I want to reoptimise.

              ; (extend?)
              ; (loop []
              ;   (let [extension (suggest-extention equation subexprs-data)]
              ;     (map
              ;       (fn [] )
              ;       {:vars extension})

              ; ; e.g. extension (= y (+ (sin (+ v x)) v2)) ;
              ; ; Like mean sqr error except that it will do it for each datapoint
              ; What if there's more than one value?  Assume unique
              ; ; so somehow I find 'v1 [1 2 3] 'v2 [2 3 4]

              ; try to find any variables in extension
              ; for all variables try to construct new dataset
              ; if possible recurse with new-dataset
              ; (find-expr subexprs-data models)
              ; ; select part of the model to extend
              ; ; select a binary extension
              ; ; select position of part to extend in binary extension
              ; ; modify the equation
              ; ; e.g. y=sin(x) +c -> y=sin(v*x)+c
              ; ; try to find new dataset v
              ; ; if I can:
              ;   ; then recurse, constraining data to have to include v
              ;   ; otherwise abandon this modification
              ; (extend the model the model)
              ; If I can't find a good extension then I might one to try a new model, or a I might want to just accept this model
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

(def linear-model
  {:as-expr '(= y (+ c (* m x)))
   ; :rhs-as-lambda
   ; (fn [x c m]
   ;   (+ c (* m x)))
   ; :rhs-arg-map {'x 0 'c 1 'm 2}
   ; :lhs-as-lambda
   ; (fn [y]
   ;  ; (println "MODEL!!" param-map indep-vars)
   ;   y)
   ; :lhs-arg-map {'y 0}

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
(def data (gen-data-uniform line 1 100 10))
(def subsexprs (gen-subexprs data 2))

; (def okb (:subexprs-data subsa))
; (println "C" (bind-data-to-model okb linear-model))

; ; (println "oakdaok" (first okb));(concat (:vars (first okb))  (:vars (second okb))))

; (println "OK" (reduce #(concat (:vars %1) (:vars %2)) okb))

(find-expr data models) 

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