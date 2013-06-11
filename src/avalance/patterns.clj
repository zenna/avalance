(ns ^{:doc "Pattern matching from Norvig."
      :author "Zenna Tavares"}
  avalance.patterns)

(def fail
  "indicates pat-match failure"
  nil)

(def no-bindings
  "indicates pat-match success wo variables"
  {})

(defn get-binding
  "Find a variable value pair in the binding list"
  [var bindings]
  (bindings var))

; (defn binding-val (binding))

(defn extend-bindings
  "Adds a var binding to binding map"
  [var input bindings]
  (assoc bindings var input))

(defn match-variable
  "Does variable match input?"
  [var input bindings]
  (let [binding-val (get-binding var bindings)]
    (cond (nil? binding-val) (extend-bindings var input bindings)
      (= input binding-val) bindings
      :else fail)))

(defn variable?
  "is x a variable (a symbol starting with ?"
  [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn segment-pattern?
  "is this a segment matching pattern?"
  [pattern]
  (and (list? pattern)
       (.startsWith (name (first pattern)) "?*")))

(defn pat-match
  "Match pattern agaisnt input in the context of the binding"
  [pattern input bindings]
  (cond
    (= bindings fail) fail
    
    (variable? pattern)
    (match-variable pattern input bindings)

    (= pattern input)
    bindings

    (segment-pattern? pattern)
    (segment-match pattern input bindings)

    (and (list? pattern) (list? input))
    (pat-match (rest pattern) (rest input)
               (pat-match (first pattern) (first input)
                           bindings))

    :else
    fail))