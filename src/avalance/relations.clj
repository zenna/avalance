(ns ^{:doc "Symbolic Regression with good proposals"
      :author "Zenna Tavares"}
  avalance.relations
  (:use clojure.test)
  (:use clojure.walk)
  (:use avalance.suggest)
  (:use clojure.math.numeric-tower)
  (:use avalance.equations)
  (:use clozen.neldermead)
  (:use clozen.helpers))

; Terms: subexpr - a function of the data, which when evaluated will return a real number
        ; model   - a predicate expression containing parameters as well as variables, cannot be evaluated unless params are instantiated
        ; equation - a (set of) models with params instatiated to values and variables to (functions of) data variables

; TODO
; Handle nil!
; Figure out what is causing NaN
; Do not repeat subexprs
; Do not repeat model
; Im overriding e0 sometimes

; Returns two vectors or tuples for each datapoint
(defn gen-data-uniform
  "Uniformly generates real valued data from function"
  [f min-val max-val num-samples]
  {:pre [(> max-val min-val)]}
  (loop [x min-val xs [] fxs []]
    (cond (> x max-val)
    {'a xs 'b fxs}
    
    :else
    (recur (+ x (/ (- max-val min-val) num-samples))
           (concat xs [x])
           (concat fxs [(f x)])))))

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

(defn expr-unique?
  [expr expr-data exprs]
  "Is this expr not already in the list of exprs"
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
  "Take a datapoint, var:val e.g. a:1.1 b:2.2,
  and an arg-map var:pos e.g. b:0 a:1
  and return an argument list, e.g. [2.2 1.1]"
  [datapoint arg-map]
  (map #(datapoint (first %1)) arg-map))

(defn place-args
  "Take a datapoint, var:val e.g. a:1.1 b:2.2,
  and an arg-map var:pos e.g. 0:b 1:a
  and return an argument list, e.g. [2.2 1.1]
  Different from rearrange args in that argmap is
  pos:var"
  [datapoint arg-map]
  (for [i (range (count arg-map))]
    (datapoint (arg-map i))))

(defn transform-data
  "Applies subexpr to data to generate new dataset"
  [expr data]
  (let [data-vars (keys data)
        data-length (count (data (first (keys data))))]
    ; Extract corresponding datapoints, e.g. a1 b1 c1 and transform through compound
    (vec (for [index (range data-length)
          :let [datapoint (zipmap data-vars
                                  (map #(nth (data %) index) data-vars))]]

      ; here ill have a:0 b:4  and function a:1 b:0
      (apply (:as-lambda expr) (rearrange-args datapoint (:arg-map expr)))))))

; TODO - DELETE THIS, appears to be redundant with make-model-lambda
(defn build-expr-metadata
  [expr data-vars]
  "Convert a subexpression to one with executable lambda and argument map"
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
    (vec (for [symb data-vars
          :when (in? flat-expr symb)]
      symb))]
    ; We need to find out which data variables are actually in the expression
    {:as-expr expr
     :as-lambda (make-lambda-args expr vars-in-expr)
     ; arg-map is maps data variable to position in argument list e.g. {'a 0 'b 1}
     ; it must be sorted for convenience in rearrange-args 
     :arg-map (sort-by val < (zipmap vars-in-expr (range (count vars-in-expr))))
     :vars vars-in-expr}))

(defn make-model-lambda-unmem
  "Make an expression evaluable
  Takes a list of arguments and return a functions and mapping from
  those arguments to position in argument list for each arg"
  [expr & args]
  ; 1. find out which of the vars or params are in the subexpr
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
          (vec (for [symb (reduce concat args)
                :when (in? flat-expr symb)]
            symb))
        arg-map (zipmap vars-in-expr (range (count vars-in-expr)))]
  ; (println "EXPR" expr "\nvars!" vars "\nparams" params "\nargmap" (sort-by val < arg-map)) 
  {:as-lambda  (make-lambda-args expr vars-in-expr) :arg-map (sort-by val < arg-map)}))

(def make-model-lambda (memoize make-model-lambda-unmem))

; TODO: this is a replacement for function above
;  change is that argmap is now map from position to variable
; instead of the other way around.  This is the more sensible thing
; but want to avoid breaking old code for now so just duplicate
(defn make-model-lambda-rpl-unmem
  "Make an expression evaluable
  Takes a list of arguments and return a functions and mapping from
  those arguments to position in argument list for each arg"
  [expr & args]
  ; 1. find out which of the vars or params are in the subexpr
  (let [flat-expr (if (symbol? expr) (list expr) (flatten expr))
        vars-in-expr
          (vec (for [symb (reduce concat args)
                :when (in? flat-expr symb)]
            symb))
        arg-map (zipmap (range (count vars-in-expr)) vars-in-expr)]
  ; (println "EXPR" expr "\nvars!" vars "\nparams" params "\nargmap" (sort-by val < arg-map)) 
  {:as-lambda  (make-lambda-args expr vars-in-expr) :arg-map arg-map}))

(def make-model-lambda-rpl (memoize make-model-lambda-rpl-unmem))

(defn extension-var?
  "Is this symbol an extension var?
  (i.e. does it start with an e)"
  [symb]
  (and (symbol? symb)
        (.startsWith (name symb) "e")))

(defn gen-subexprs
  "Generate functions of data variables,
  use expr-constraints to restrict generated expressions
  through rejection sampling (hence must be relatively easily
  satisfiable)"
  [data num-to-gen expr-constraints? mandatory-exprs seen-subexprs]
  ; (println "---GENERATING EXPRESSIONS")
  (let [data-vars (vec (keys data))
        ; Remove extensions from grammar because we dont want recursive definitions
        data-vars-wo-extensions (filter #(not (extension-var? %1)) data-vars)
        data-prods (map (fn [variable] {:prod variable :weight 10000.0}) data-vars-wo-extensions)
        new-pcfg (add-rule-to-pcfg data-prods compound-pcfg 'V)

        ; Generate exprs through sample and reject if it fails to adhere to constraints
        expr-exprs-data
          (loop [exprs {} num-left-to-gen num-to-gen mandatory-exprs-loop mandatory-exprs]
            (let [expr (if (empty? mandatory-exprs-loop)
                           (gen-expr-pcfg new-pcfg)
                           (first mandatory-exprs-loop))
                  mandatory-exprs-loop (rest mandatory-exprs-loop)
                  expr-exec (build-expr-metadata expr data-vars)
                  ; ok (println "EXPRESSION IS" expr "NANDS" mandatory-exprs "mand!" mandatory-exprs-loop)
                  expr-data {expr (transform-data expr-exec data)}]
                  ; ok (println "COVARIES" (expr-covaries? expr-data data) "UNIQUE" (expr-unique? expr expr-data exprs))]
              (cond
                (zero? num-left-to-gen)
                exprs

                ; Check that the data covaries with the data and is unique
                (and (expr-covaries? expr-data data)
                     (expr-unique? expr expr-data exprs))
                     ; (expr-constraints? expr (if-let [a (keys exprs)] a '())))
                (recur (merge exprs expr-data) (dec num-left-to-gen) mandatory-exprs-loop)

                :else
                (recur exprs num-left-to-gen mandatory-exprs-loop))))]
          ; ok (println "SEEN" seen-subexprs "NEW" (keys expr-exprs-data))]
;
    ; (println "VARS" data-vars "A" seen-subexprs "B" (keys expr-exprs-data))

    ; Try again if I generated something I've already seen
    (cond
        ; hack to prevent infinite loops when num-to-gen is 1 and already seen e0
        (and (not (empty? mandatory-exprs))
             (in? seen-subexprs (keys expr-exprs-data)))
        (recur data 2 expr-constraints? mandatory-exprs seen-subexprs)

        (in? seen-subexprs (keys expr-exprs-data))
        (recur data num-to-gen expr-constraints? mandatory-exprs seen-subexprs)

        :else
        ; Otherwise package it up nicely
        {:subexprs
         ; PERFORMANCE: MEMOIZE build-expr-metadata
         (map #(build-expr-metadata %1 data-vars) (keys expr-exprs-data))

         :subexprs-data
         expr-exprs-data})))

(defn gen-subexprs-data
  "Generate data from subexpressions and data"
  [data subexprs]
  (zipmap subexprs
          (map #(transform-data (build-expr-metadata % (keys data)) data) subexprs)))

(defn sum-sqr-error
  "Take a model and a dataset and produce a function which when given a set of 
  parameters of model will compute the SSE of the model against data"
  [model data var-binding]
  ; var-binding e.g. {a: x b: y}
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

(defn cost-to-score
  "convert a cost to a score"
  [model cost data]
  ; (println "CONVERTING COST TO SCORE" cost (reciprocal cost))
  (if (Double/isNaN cost) ; Handle NaN
    Double/POSITIVE_INFINITY
    cost))

(defn fit-model
  "Returns best fit model against data"
  [data model var-binding]
  (let [cost-func (sum-sqr-error model data var-binding)
        num-params (count (:params model))
        best-fit-params (nelder-mead cost-func (vec (repeatedly num-params rand)))]
    (merge model
      {:param-values (zipmap (:params model) (:vertex best-fit-params))
       :score (cost-to-score model (:cost best-fit-params) data)
       :var-binding var-binding})))

(defn create-cost-func-for-datapoint
  [datapoint model var-binding lhs-lambda rhs-lambda]
  "For a data point, generate a cost function taking |extensions| args
  and evaluate to 0 when lhs = rhs for that datapoint"
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
  ; (println "FITTING EXTENSIONS")
  (let [data-size (count (data (first (keys data))))
        lhs (nth (:as-expr-ext equation) 1)
        rhs (nth (:as-expr-ext equation) 2)
        lhs-lambda (make-model-lambda lhs (:vars equation) (:params equation) (:ext-vars equation))
        rhs-lambda (make-model-lambda rhs (:vars equation) (:params equation) (:ext-vars equation))
        ; pbuddy (println "DADA" data-size equation lhs rhs)

        ; For every data point, run nelder mead
        extension-vals (for [data-index (range data-size)
                              :let [slice (slice-data data data-index)
                                    ; Create a new cost function for each data point
                                    cost-func (create-cost-func-for-datapoint slice
                                                equation var-binding lhs-lambda rhs-lambda)]]
                          ; USE NELDER-MEAD to find PARAMETERS FOR THIS POINT
                          (nelder-mead cost-func
                                      (vec (repeatedly (count (:ext-vars equation)) rand))))]
      
      ; We end up with lots of values, need to compile together to create single cost
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

(defn bind-data-to-model
  "Creates a mapping between the variables of a sub expression and those of model"
  [data model mandatory-exprs]
  {:pre [(= (count (keys data)) (count (:vars model)))]}
  ; (println "MANDS" mandatory-exprs)
  (let [indep-var (nth (:as-expr model) 1)
        var-binding (if (not (empty? mandatory-exprs))
                        {indep-var (first mandatory-exprs)}
                        {})
        new-vars (remove #(in? (keys var-binding) %) (:vars model))
        new-data (remove #(in? (vals var-binding) %) (keys data))]
  ; (println "DATA" data "MODEL" model)
  (merge var-binding (zipmap (shuffle new-vars) new-data))))

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
            (let [all-keys (coll-to-keys equation (fn [elem pos] (or (zero? pos)
                                                                     (extension-var? elem))))
                  key-to-change (rand-nth (keys all-keys))
                  error-f (rand-nth error-fs)
                  value-at-key (all-keys key-to-change)
                  ext-name (nth extension-vars extension-num)
                  rand-replacement (rand-nth [(list error-f value-at-key ext-name)
                                             (list error-f ext-name value-at-key)])
                  new-equation (replace-in-sublist modified-equation key-to-change rand-replacement)]
              (recur new-equation (inc extension-num)))))]

    ; TODO: We need a better way to stop latter extensions overriding previous ones
    ; Hack for now: just check which extensions actually end up in the expression
    {:as-expr-ext extended-expr :ext-vars (filter #(in? (flatten extended-expr) %) extension-vars)}))

(defn make-expr-constraints
  [exprs-constraints]
  "Create a function which checks a given expression and previously
  generated expressions and say's whether it's alright or not.
  exprs-constraints: vector of all symbols which must be in equation"
  (fn [current-expr gend-exprs]
    ; We transformed the generated expressions to 
    ; account for the fact that single symbols will not be lists
    ; a bit of a hack
    (let [new-gend-exprs (map #(if (symbol? %)
                                    (list %)
                                    %)
                            gend-exprs)
          outcome
      (if
        ; is it true that each constraint exists in the gend exprs
        (every? (fn [c] (some #(in? %1 c) new-gend-exprs)) exprs-constraints)
        true
        ; Otherwise is it true that the proposed expression contains
        ; any of the constraint expressions
        ; And that not all of these expressions are already in gend
        (if-let [constraints-in-current
                 ; Again must handle case proposed expression may be symbol or list
                 (empty-to-nil (filter  (fn [constraint]
                                          (if (symbol? current-expr)
                                              (= constraint current-expr)
                                              (in? current-expr constraint)))
                                        exprs-constraints))]
          (not-every? #(in? gend-exprs %1) constraints-in-current)
          false))]
          ; ok (println "TESTING CONSTRAINTS MOFO" current-expr gend-exprs outcome)]
      
      outcome)))

(declare find-expr)

(defn find-good-extensions
  "Find some good extensions"
  [model all-models error-fs equation var-binding subexprs-data data depth]
  (loop [good-extensions {} num-tries-left 10]
    (cond
      (zero? num-tries-left)
      good-extensions

      ; Let's try to find an extension
      :else
      (let [prefixprint (apply str (repeat depth "  "))
            ok (println "\n" prefixprint "Trying extension" num-tries-left)
            extended-expr (suggest-extension (:as-expr model) error-fs)
            extended-model (merge model extended-expr {:param-values (:param-values equation)})
            {extension-data :vertex score :cost} (fit-extensions subexprs-data extended-model var-binding)]
            ; (println extension-data " score " score)

        ; If the score deviates from zero, we couldn't fit
        ; a good extension dataset, so let's skip
        (if (> score 0.0001)
            (recur good-extensions (dec num-tries-left))

            ; Otherwise let's see if we can make a fit the extension by recursing with
                  ; legacy hack: make-expr-constraints expects a vector of symbols,
                  ; so for converts 'e0 to ['e0] for instance 
            (let [compound-data (merge (zipmap (:ext-vars extended-expr) extension-data) data subexprs-data)
                  extensions (map (fn [constraint]
                                    (let [eqs (find-expr compound-data all-models error-fs (inc depth) constraint)]
                                      (if (empty? eqs)
                                          nil
                                          (rand-nth-reciprocal-categorical eqs (extract eqs :score)))))
                                  (for [symb (:ext-vars extended-expr)] [symb]))
                  ok (println prefixprint "Found could fit extension to data - Recursing to find expression for extension" (:as-expr-ext extended-expr))
                  ; extension (find-expr compound-data all-models error-fs (inc depth) expr-constraints)
                  found-ext-all-vars (not-any? nil? extensions)
                  ; okok (println "Found Extensions:" (count extensions) "END")
                  ; okok (println "Found Extensions2:" extensions "END")
                  ]
                  ; (println "ARE"extension)
                  (if found-ext-all-vars ;TOOD extension is good?
                  (recur (merge good-extensions {extensions extended-expr}) (dec num-tries-left))
                  (recur good-extensions (dec num-tries-left)))))))))

; Selection of models should be based on the data, and the available models
(defn sample-model
  "choose models, currently random"
  [sampled-models models data]
  {:pre [(< (count sampled-models) (count models))]}
  (let [proposed-model (rand-nth models)]
    ; Don't repeat, don't choose one i've seen before
    ; (println "sampled-models" sampled-models)
    (if (in? sampled-models proposed-model)
        (recur sampled-models models data)
        proposed-model)))

(defn accept-equation?
  [equation data]
  ; (println "EQUATION" equation)
  (cond
    (< (:score equation) 0.05) true

    :else false))

; Stop if the depth get's greater than some threshold
(defn extend?
  "Should we extend?"
  [depth equation]
  ; (println "extend? score is" (:score equation) " Depth:" depth)
  (if (and (not (NaN? (:score equation))) ;This is because we sometimes get NaNs 
          (< depth 1)
          (< (:score equation) 10)) ;TODO- ARBITRARY NUMBER HERE
      true
      false))

(defn try-more-subexprs?
  [equations num-plots-left]
  ; (println "Try More Subexpressions? Current Equations:" equations)
  (cond
    (zero? num-plots-left) false

    (empty? equations) true

    ; Stop recursing if any of them are really good
    (some #(accept-equation? %1 'FIXME) equations)
    false

    :else true))

; If we are to say a model is a set of equations then well, a lot of this structure need be updated
(defn add-ext-to-model
  "incorporate an extension into a model"
  [model extension expr]
  ;(println "INCORPORATING MODEL" model "\nEXTENSION " extension "expr" expr)
  (let [
  updated-exts
  ; For every extension var find its expression with vars/params updated to avoid conflicts
  (map (fn [ext-var]
          ; First find the relative extension equation for this extension var
          (let [ext (first (filter (fn [eq]
                                    (let [indep-var (nth (:as-expr eq) 1)
                                          indep-var-bind ((:var-binding eq) indep-var)]
                                          (= indep-var-bind ext-var)))
                            extension))
                ; Then get the extended expression RHS
                ext-expr-rhs (nth (:as-expr ext) 2)
                ; Update this expression - For each sybmol
                new-ext-expr-rhs (walk-msg (fn [elm msg]
                                             (cond

                                             ; Is it a parameter? if so we need to rename
                                             ; to avoid name clashe, names n
                                             (in? (:params ext) elm)
                                             (let [new-elm (gen-until #(symbol (str elm "-" ext-var "-" (rand-int 10000)))
                                                                      #(not (in? (:params model) %)))
                                                   param-value (elm (:param-values ext))
                                                   param-values {:param-values {new-elm param-value}}
                                                   msg (merge-with #(merge %1 %2) msg param-values)]
                                             {:elm new-elm :msg msg})
                                             

                                             (in? (:vars ext) elm)
                                             (let [new-elm (gen-until #(symbol (str elm "-" ext-var "-" (rand-int 10000)))
                                                                      #(not (in? (:vars model) %)))
                                                   var-binding ((:var-binding ext) elm)
                                                   bind {:var-binding {new-elm var-binding}}
                                                   msg (merge-with #(merge %1 %2) msg bind)]
                                             {:elm new-elm :msg msg})

                                             :else
                                             {:elm elm :msg msg}))
                                  ext-expr-rhs)]
                new-ext-expr-rhs))
    
    (:ext-vars expr))
    ;ok (println "UPDATED EXTS" updated-exts)
    var-binding (merge (model :var-binding)
                       (reduce merge (extract-in updated-exts [:msg :var-binding])))
    param-values (merge (model :param-values)
                       (reduce merge (extract-in updated-exts [:msg :param-values])))
    smap (zipmap (:ext-vars expr) (extract updated-exts :coll))
    final-expr (postwalk-replace smap (:as-expr-ext expr))]
    {:var-binding var-binding :vars (keys var-binding)
     :param-values param-values :params (keys param-values)
     :as-expr final-expr}))
  ;1. for each ext-var
      ; Find the relevant extension in extension)

(defn compile-expr
  "Compile data and parameters into expression"
  [expr]
  (postwalk-replace (merge (:var-binding expr) (:param-values expr)) (:as-expr expr)))

(defn find-expr
  "Searches for an expression"
  [data all-models error-fs depth mandatory-exprs]
  (let [max-num-plots 5
        prefixprint (apply str (repeat depth "  "))
        ok (println "\n" prefixprint "RECURSING WITH DATA" (keys data))]

    ; loop over (samples of) subexpression sets
    (loop [equations [] num-plots-left max-num-plots seen-subexprs []]
      (let [num-subexprs-attempt 2;(inc (rand-int 2))
            expr-constraints? (make-expr-constraints mandatory-exprs)
            subexprs-subexprs-data (gen-subexprs data num-subexprs-attempt expr-constraints? mandatory-exprs seen-subexprs)
            {subexprs :subexprs subexprs-data :subexprs-data} subexprs-subexprs-data
            num-subexprs (count (keys subexprs-data))
            ; Filter out models of wrong number of parameters
            models (filter #(= num-subexprs (count (:vars %))) all-models)
            ok (println "\n" prefixprint "Sub-expressions:" num-subexprs (keys subexprs-data))
            equations
        (into equations
        ; Loop through different models return set of
        (loop [sampled-models [] c-equations [] num-model-tests-left (count models)]
          (cond
            (zero? num-model-tests-left)
            c-equations

            (= (count sampled-models) (count models))
            c-equations

            :else
            (let [; pvar (println "C-EQUATIONS" c-equations)
                  model (sample-model sampled-models models subexprs-data)
                  sampled-models (conj sampled-models model)
                  var-binding (bind-data-to-model subexprs-data model mandatory-exprs)
                  ; pvar (println "VARBINDING" var-binding)
                  equation (fit-model subexprs-data model var-binding)
                  ; pvar (println prefixprint "TRIED MODEL" (vals (:model equation)) (:cost equation))
                  okS (println prefixprint "Tried Model, Depth: " depth " Got equation" (vals equation))]
                                
              ; Should I extend the model or not?
              (cond
                (accept-equation? equation subexprs-data)
                (recur sampled-models (conj c-equations equation) (dec num-model-tests-left))

                ; Shall I attempt an extension? Impose hard depth
                (extend? depth equation)
                (let [good-extensions-all (find-good-extensions model
                                        all-models error-fs equation
                                        var-binding subexprs-data
                                        data depth)]
                      (if (empty? good-extensions-all)
                        (recur sampled-models c-equations (dec num-model-tests-left))
                        (let [good-extensions (keys good-extensions-all)
                              good-ext-exprs (vals good-extensions-all)
                              ;For each proposed extension incorporate into model, refit model, and sample-a-good-
                              whatwhat (println "Found good Extensions" good-extensions "\n")
                              weights (map (fn [ext] (mean (extract ext :score)))
                                            good-extensions)
                              sampled-good-extension (rand-nth-reciprocal-categorical good-extensions weights)
                              good-ext-expr (good-extensions-all sampled-good-extension)
                              extended-model (add-ext-to-model equation sampled-good-extension good-ext-expr)
                              ok (println "extended model Final" extended-model)
                              ;new-vars (println "NEWVARS!!" (:vars extended-model))
                              new-subexprs-data (gen-subexprs-data data (vals (:var-binding extended-model)))
                              ;ko (println "NEWDATA" new-subexprs-data)
                              extended-model-refit (fit-model new-subexprs-data extended-model (:var-binding extended-model))
                              ok (println "Scored extended model Final" extended-model-refit)]

                          ; If we found a good extension, incorporate into the equations, add this to the list
                          ; and then try a new model
                          (recur sampled-models (conj c-equations extended-model-refit) (dec num-model-tests-left)))))

                ; If I am not stopping nor extending I should try a new model
                :else
                (recur sampled-models c-equations (dec num-model-tests-left)))))))]
        
        ; Should I try more subexprs
        (cond
          (try-more-subexprs? equations num-plots-left)
          (recur equations (dec num-plots-left) (conj seen-subexprs (keys subexprs-data)))

          :else
            equations)))))

(defn new-find-expr
  "Searches for an expression"
  [data all-models mandatory-exprs attrs]
  (let [max-num-plots 5
        model-attr-vals (map #(find-attr-vals % attrs) all-models)]
        model-attr-vals))

    ; ; loop over (samples of) subexpression sets
    ; (loop [equations [] num-plots-left max-num-plots seen-subexprs []]
    ;   (let [num-subexprs-attempt (inc (rand-int 2))
    ;         expr-constraints? (make-expr-constraints mandatory-exprs)
    ;         subexprs-subexprs-data (gen-subexprs data num-subexprs-attempt expr-constraints? mandatory-exprs seen-subexprs)
    ;         {subexprs :subexprs subexprs-data :subexprs-data} subexprs-subexprs-data
    ;         num-subexprs (count (keys subexprs-data))
    ;         models (filter #(= num-subexprs (count (:vars %))) all-models)
    ;         equations (suggest-ext subexprs-data models attrs)]
    ;     model-attr-vals))))