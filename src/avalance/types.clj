(ns avalance.types)

; 1. Implement tv
; 2. Make robust, test
; 3. do consistency mathign in func-matchaes-args
; 4. Write transformation 

; every type is a list where the first value is one of
; fun, list, tuple, type-var, prim
; the remaining values depend on the first value:
; fun: first value is another list where each elemenet is the argument
;      second value is a type of return argument 
(defn combine_bindings
  [binding-1 binding-2]
  (let [types-match? (and (first binding-1) (first binding-2))
        bindings (vec (concat (second binding-1) (second binding-2)))]
        [types-match? bindings]))

(declare same-type?)

(defn get-bindings
  "Get the bindings yo"
  [func-t args-t bindings]
  ; Assune funcs-t and args-t are same size, should check for this
  (if (= (count func-t) 1)
    (same-type? (first func-t) (first args-t) bindings)
    (recur (rest func-t)
           (rest args-t)
           (same-type? (first func-t) (first args-t) bindings))))

(defn func-matches-args?
  "Is a function type-consistent with a set of arguments"
  [func-t args-t]
  (let [func-args-t (second func-t) bindings [true []]]
    (get-bindings func-args-t args-t bindings)))

(defn same-type?
  [t-1 t-2 bindings]
  ""
  (let [t-1-class (first t-1)
        t-2-class (first t-2)
        failed_binding [false []]]
    (cond
      (and (= t-1-class 'tv) (not= t-2-class 'tv))
        (combine_bindings bindings [true [(second t-1) t-2]])

      (and (= t-2-class 'tv) (not= t-1-class 'tv))
        (combine_bindings bindings [true [(second t-2) t-1]])

      ; If they are both type variables then do nothing
      (and (= t-1-class 'tv) (= t-2-class 'tv)) bindings

      ; If neither is tvs and their types-classes mismatch, something's wrong
      (not= t-1-class t-2-class) failed_binding
      
      ; If both functions, recurse with arguments and return value
      (= t-1-class 'fun)
        (let [t-1-args-ret (conj (second t-1) (last t-1))
              t-2-args-ret (conj (second t-2) (last t-2))]
              (get-bindings t-1-args-ret t-2-args-ret bindings))
      
      ; If they are both lists then let's look at the list element
      (= t-1-class 'lst)
        (let [t-1-elem-type (second t-1)
              t-2-elem-type (second t-2)]
          (same-type? t-1-elem-type t-2-elem-type bindings)))))

; (def reduce-t '(fun ((fun a b) (list a))
;                      (list b)))
(def map-t '(fun ((fun ((tv a)) (tv b))
   (lst (tv a))) 
  (lst (tv a))))

(def inc-t '(fun ((num)) (num)))

(def list-int-t '(lst (num)))


(defn -main
  []
  (func-matches-args? map-t (list inc-t list-int-t)))

(require 'avalance.types)
(use 'clojure.tools.trace)
(trace-ns 'avalance.types)
(-main)