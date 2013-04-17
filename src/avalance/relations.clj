; Symbolic Regression with good proposals
(ns avalance.relations
  (:require [clojure.math.combinatorics :as combo])
  (:use clojure.test))

(use 'clojure.math.numeric-tower)
(use 'avalance.equations)
(use 'avalance.neldermead)
(use 'avalance.helpers)

; What's wrong with this

; 1.I need a better structure for attempting things, and adding things that I can attempt without getting into
; nested nested loops
; 2. Keeping track of goood things
; 3. The hard analysis of what's wrong, what extensions to make
; 4. Learning?
; - Smarter generation of expressions
; - A PCFG is problematic, I want to try simple things first

; TODO
; THE INFERENCE
; Handle nil!
; Return right data structure from gen thing
; Implement expression testing to prevent stupid expressions
; bind variables to model
; update cost func to take into account new model form
; decide accept criterion
; write code to
; choose sub expression of 
; AVOID TRYING TO FIT PARAMS WHEN THERE ARE NONE

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

; FIXME: It could be that the data covaries in wuch a way that function
(defn expr-covaries?
  [expr-data data]
  "Does the exprs covary with the data i.e. is not constant"
  (let [data-values (first (vals expr-data))
        test-point (first data-values)]
        ; (println "testpoint" test-point "datavals" data-values)
    (loop [data-values data-values does-covary true]
      (cond (empty? data-values)
        does-covary

        (tolerant= (first data-values) test-point)
        (recur (rest data-values) false)

        :else
        true))))

; 
(defn expr-unique?
  [expr expr-data exprs]
  "Is this exprs not already in the list of exprs"
  ; Check syntactic equivalence first, as less expensive
  (cond
    (in? (keys exprs) expr)
    false

    :else
    (loop [exprs-loop (keys exprs)]
      (cond
        (empty? exprs-loop) true

        ; if different
        (not-every? true?
          (vec-f #(tolerant= %1 %2)
            (exprs (first exprs-loop))
            (first (vals expr-data))))
        (recur (rest exprs-loop))

        :else
        false))))

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
        data-prods (map (fn [variable] {:prod variable :weight 10000.0}) data-vars)
        new-pcfg (add-data-to-pcfg data-prods compound-pcfg)
        ; Generate exprs through sample and reject if it fails to adhere to constraints
        expr-exprs-data
          (loop [exprs {} num-left-to-gen num-to-gen]
            (let [expr (gen-expr-pcfg new-pcfg)
                  expr-exec (build-expr-metadata expr data-vars)
                  expr-data {expr (transform-data expr-exec data)}]
              (cond
                (zero? num-left-to-gen)
                exprs

                ; Check that the data covaries with the data and is unique
                (and (expr-covaries? expr-data data) (expr-unique? expr expr-data exprs))
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
  [expr & args]
  ; 1. find out which of the vars or params are in the subexpr
  ; TODO FIX THIS SUCH THAT PARAMETERS ALWAYS COME FIRST
  ; (println "EXPR" expr "args" args)
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
          (vec (for [symb (reduce concat args)
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

; Returns [what is love! baby dont hurt me]
; TODO generalise this to n params
(defn fit-model
  "Returns error for best fit of all models against data"
  [subexprs data model var-binding]
  (let [cost-func (sum-sqr-error model data var-binding)
        best-fit-params (nelder-mead cost-func [1.0 1.0])]
    {:model model
     :param-values (zipmap (:params model) (:vertex best-fit-params))
     :cost (:cost best-fit-params)
     :var-binding var-binding}))

(defn create-cost-func-for-datapoint
  [datapoint model var-binding lhs-lambda rhs-lambda]
  (fn [extension-values]
    (let [extension-binding (zipmap (:ext-vars model) extension-values) 
          {expr :as-expr vars :vars params :params} model]
          ; (println "param-binding" param-binding "\n\n datasize" (:lhs-arg-map model))
          (let [lhs-rhs
            (map
              (fn [model-side]
                ; (println "mdoel-side" model-side "WHAT?\n" (:arg-map model-side) "\n")
                ; args = list of arguments for side of equation
                ; found by 
                (let [args (for [arg (keys (:arg-map model-side))
                          :let [value (cond
                                          ; if it's a variable, look it up in the datapoint
                                          (in? (:vars model) arg)
                                          (datapoint (var-binding arg))

                                          ; if it's a parameter look it up in the param values
                                          (in? (:params model) arg)
                                          ((:param-values model) arg)

                                          ; Otherwise it must be an extension
                                          :else
                                          (extension-binding arg))]]
                          value)]
                    ; (println "args!!!!" args "keys!!!" (keys (:arg-map model-side) ))
                  ; (println ["a" args "\nms" model-side "\npb" extension-binding])
                  ; (println (if (nil? (first args)) ["a" args "\nms" model-side "\npb" param-binding "\nds" data-size "\ndata" data "\nvb" var-binding] '() ))
                  (apply (:as-lambda model-side) args)))
              
              [lhs-lambda rhs-lambda])]

      (Math/pow (- (first lhs-rhs) (second lhs-rhs)) 2)))))

; PERFORMANCE THIS IS SURELY VERY INEFFIICIEINT
(defn slice-data
  "Convert {x [1 2 3] y [2 3 4]} to {x 1 y 2} when data-index = 0"
  [data data-index]
  (reduce
    merge
    (for [data-var (keys data)]
      {data-var (nth (data data-var) data-index)})))

(deftest slice-data-test
  (let [data {'a [1 2 3] 'b [4 5 6]}
        expected-result {'b 6, 'a 3}]
    (is (= (slice-data data 2) expected-result))))

; Returns {'e0 [1 2 3] 'e1 [3 4 5]}
(defn fit-extensions
  "Takes an equation with extensions and returns a new dataset for each extension"
  [data equation var-binding]
  (println "FITTING EXTENSIONS")
  (let [data-size (count (data (first (keys data))))
        ; For every data point, generate a cost function taking |extensions| args
        ; and evaluate to 0 when lhs = rhs for that datapoint
        ; Make model lambda only needs to happen once per expression
        ; it produces a function for each side of the equation which takes as input the variables
        ; parameters and an extension value
        ; the make cost function shold take a made lambda
        ; and return a function which takes in a set of values for the extensions
        ; takes the values for a particular datapoint
        ; computes the left and right hand sides
        
        lhs (nth (:as-expr-ext equation) 1)
        rhs (nth (:as-expr-ext equation) 2)
        lhs-lambda (make-model-lambda lhs (:vars equation) (:params equation) (:ext-vars equation))
        rhs-lambda (make-model-lambda rhs (:vars equation) (:params equation) (:ext-vars equation))
        ; pbuddy (println "DADA" data-size equation lhs rhs)

        extension-vals (for [data-index (range data-size)
                              :let [slice (slice-data data data-index)
                                    ; Create a new cost function for each data point
                                    cost-func (create-cost-func-for-datapoint slice
                                                equation var-binding lhs-lambda rhs-lambda)]]
                          ; USE NELDER-MEAD to find PARAMETERS FOR THIS POINT
                          (nelder-mead cost-func
                                      (vec (repeatedly (count (:ext-vars equation)) rand))))]
      (reduce (fn [all r]
                  (let [all-v (:vertex all)
                        r-v (:vertex r)
                        all-s (:cost all)
                        r-s (:cost r)]
                  {:vertex
                  (for [ind (range (count all-v))
                        :let [i (nth all-v ind)
                              j (nth r-v ind)]]

                    (if (vector? i)
                        (conj i j)
                        [i j]))
                  :cost (+ all-s r-s)}))

        extension-vals)))

; [1 2] [1 2] -> [[1 1] [2 2]]
; [[1 1] [2 2]] -> [1 2]
; (def extension {:as-expr (= y (+ (sin (+ v x)) v2)) :params ['v1 'v2] :vars ['y 'x]
; Now the difference is that I am not trying to fit one v but create a new dataset
; 1. I need to create a new rhs-lambda lhs-lambda
; 2. I could just make a function which makes the rhs-lhs lambda from a model
; 3. But first update the model by insantating the parameters and adding in the vs
; 4. Then I need to abstract out details of mean-sqr-error and make another function which



; (defn accept-equation?
;   "Should we continue down this path"
;   ;. If no more tests left then accept of course
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
; FIXME: This can replace extension
(defn suggest-extension
  "Suggest an extension to an equation"
  [equation error-fs]
  (let [num-extensions (inc (rand-int 2))
        extension-vars (map #(symbol (str "e" %)) (range num-extensions))
        extended-expr
        ; Repeatedly reply a modification
        (loop [modified-equation equation extension-num 0]
          (if (= num-extensions extension-num)
            modified-equation
            ; Ignore first elements of list, which will be function symbols;
            ; We don't want to replace them as they don't evaluate by themselves to reals
            (let [all-keys (coll-to-keys equation (fn [elem pos] (zero? pos)))
                  key-to-change (rand-nth (keys all-keys))
                  error-f (rand-nth error-fs)
                  value-at-key (all-keys key-to-change)
                  ext-name (nth extension-vars extension-num)
                  rand-replacement (rand-nth [(list error-f value-at-key ext-name)
                                             (list error-f ext-name value-at-key)])
                  new-equation (replace-in-sublist modified-equation key-to-change rand-replacement)]
              (recur new-equation (inc extension-num)))))]

    {:as-expr-ext extended-expr :ext-vars extension-vars}))

(declare find-expr)

(defn find-good-extensions
  "Find some good extensions"
  [model all-models error-fs equation var-binding subexprs-data data depth]
  (loop [good-extensions [] num-tries-left 10]
    (cond
      (zero? num-tries-left)
      good-extensions

      ; Let's try to find an extension
      :else
      (let [extended-expr (suggest-extension (:as-expr model) error-fs)
            extended-model (merge model extended-expr {:param-values (:param-values equation)})
            {extension-data :vertex score :cost} (fit-extensions subexprs-data extended-model var-binding)]
            (println extension-data " score " score)

        ; If the score deviates from zero, we couldn't fit
        ; a good extension dataset, so let's skip
        (if (> score 0.0001)
            (recur good-extensions (dec num-tries-left))

            ; Otherwise let's see if we can make a fit the extension by recursing with
            (let [compound-data (merge (zipmap (:ext-vars extended-expr) extension-data) data subexprs-data)
                  extension (find-expr data all-models error-fs (inc depth))]
                  (println "ARE"extension)
                  (if true ;TOOD extension is good?
                  (recur (conj good-extensions extension) (dec num-tries-left))
                  (recur good-extensions (dec num-tries-left)))))))))

; Selection of models should be based on the data, and the available models
(defn sample-model
  "choose models, currently random"
  [sampled-models models data]
  {:pre [(<= (count sampled-models) (count models))]}
  (let [proposed-model (rand-nth models)]
    ; Don't repeat, don't choose one i've seen before
    (if (in? sampled-models models)
        (recur sampled-models models data)
        proposed-model)))

(defn continue?
  "Should we continue down this path"
  [score model num-tests-left])

; Stop if the depth get's greater than some threshold
(defn extend?
  "Should we extend?"
  [depth]
  (if (> depth 2)
      false
      true))

(defn accept-equation?
  [equations num-tests-left]
  (if (zero? num-tests-left)
      true
      false))

(defn try-more-subexprs?
  [equations num-plots-left]
  (cond
    (zero? num-plots-left) false
    :else ; TODO WHEN ELSE SHOULD I STOP TRYING SUBEXPRS
      true))

; TODO- Depth control of extensions
(defn find-expr
  "Searches for an expression"
  [data all-models error-fs depth]
  (let [num-subexprs 2;(inc (rand-int 2)); TODO this should be dependent on the number of variables in the data
        models (filter #(= num-subexprs (count (:vars %))) all-models)
        max-num-plots 20
        ok (println "RECURSING")]

    ; loop over (samples of) subexpression sets
    (loop [equations [] num-plots-left max-num-plots]
      (let [subexprs-subexprs-data (gen-subexprs data num-subexprs)
           {subexprs :subexprs subexprs-data :subexprs-data} subexprs-subexprs-data
           ok (println "SUBS" subexprs)
            equations
        (conj equations
        ; Loop through different models return set of
        (loop [sampled-models [] equations [] num-model-tests-left (count models)]
          (let [model (sample-model sampled-models models subexprs-data)
                sampled-models (conj sampled-models model)
                var-binding (bind-data-to-model subexprs-data model)
                equation (fit-model subexprs subexprs-data model var-binding)]
                ; 1. compile-expression ]
                ; (println equation)
            
            ; Should I extend the model or not?
            (cond
              (accept-equation? equations num-model-tests-left)
              equation

              ; Shall I attempt an extension? Impose hard depth
              (extend? depth)
              (let [good-extensions (find-good-extensions model
                                      all-models error-fs equation
                                      var-binding subexprs-data
                                      data depth)
                    good-extension-found true] ; TODO

                  ; If we found a good extension, incorporate into the equations, add this to the list
                  ; and then try a new model
                  (if true;(good extension-found)
                      equations;(incorporate-into-model-and-recurse) ; TODO
                      (recur sampled-models equations (dec num-model-tests-left))))

              ; If I am not stopping nor extending I should try a new model
              :else
              (recur sampled-models equations (dec num-model-tests-left))))))]
        
        ; Should I try more subexprs
        (cond
          (try-more-subexprs? equations num-plots-left)
          (recur equations (dec num-plots-left))

          :else
            equations)))))


; Models are expressions, which have parameters and variables
; Parameters are values to be optmised
; Whereas variables come from compounds of data
(def exponent-model
  {:as-expr '(= y (Math/pow b x))
  :params ['b]
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
  {:as-expr '(= y (Math/sin x))
   :params []
   :vars ['y 'x]
   :name 'sin})

(def constant-model
  {:as-expr '(= y k)
   :params ['k]
   :vars ['y]
   :name 'constant})

(def models
  [power-model
   linear-model
   exponent-model
   sin-model
   constant-model])

; Error functions - all binary
(def error-fs
  ['+ '*])

; Example function from Herb Simon
(defn kepler
  [D]
  (* 1.5 (expt D (/ 3 2))))

(defn line
  [x]
  (+ 1.5 (* 10 x)))

(defn x-squared
  [x]
  (+ (* x x) (Math/sin x)))

(defn a-little-complex
  [x]
  (+ (* x x) (* (Math/sin x) 3)))

(def dt {'a [1 2 3] 'b [10 14 12]})
(def data (gen-data-uniform x-squared 1 100 10))
(def subsexprs (gen-subexprs data 2))

; (def okb (:subexprs-data subsa))
; (println "C" (bind-data-to-model okb linear-model))

; ; (println "oakdaok" (first okb));(concat (:vars (first okb))  (:vars (second okb))))

; (println "OK" (reduce #(concat (:vars %1) (:vars %2)) okb))

(find-expr data models error-fs 0) 

(defn -main[])

; (defn -main
;   []
;   ;1. Generate data 
;   (let [data (gen-data-uniform a-little-complex 1 100 10)]
;         ; model-weights (subvec (vec (sort-by :cost (find-expr data models))) 0 5)]
;     data))

; (require 'avalance.relations)
; (use 'clojure.tools.trace)
; (trace-ns 'avalance.relations)
; (gen-data-uniform inc 3 5 10)
; (-main)