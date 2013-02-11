; 1. Implement tv
; 2. Make robust, test
; 3. do consistency mathign in func-matchaes-args
; 4. Write transformation 

:type

; every type is a list where the first value is one of
; fun, list, tuple, type-var, prim
; the remaining values depend on the first value:
; fun: first value is another list where each elemenet is the argument
;      second value is a type of return argument 
(def reduce-t '(fun ((fun a b) (list a))
                     (list b))) 

(def identity-t '(fun ((tv a)) (tv a)))

(defn combine_bindings
  [binding-1 binding-2]
  (let [types-match? (and (first binding-1) (first binding-2))
        bindings (concat (second binding-1) (second binding-2))]
        (list types-match bindings)))

(defn func-matches-args?
  "Is a function type-consistent with a set of arguments"
  [func-t args-t]
  (let [func-args-t (rest func-t) bindings [true []]]
    (get-bindings func-args-t args-t)

(defn get-bindings
  "Get the bindings yo"
  [func-t args-t bindings]
  (if (empty func-args-t)
    (same-type (first func-args-t) (first func-args-t) bindings))
    (recur (rest func-t)
           (rest arg-t)
           (same-type? (first func-args-t) (first args-t) bindings))))

(defn same-type?
  [t-1 t-2 bindings]
  ""
  (let [t-1-class (first t-1)
        t-2-class (first t-2)
        failed_binding '(false, [])]
    (cond
      (and (= t-1-class 'tv) (= t-2-class 'tv)) bindings
      (not= t-1-class t-2-class) failed_binding
      (= t-1-class 'fun)
        (let [t-1-args-ret (conj (second t1) (last t1))
              t-2-args-ret (conj (second t2) (last t2))]
              (get-bindings t-1-args-ret t-2-args-ret bindings))
      (= (t-1-class) 'list)
        (let [t-1-elem-type (second t-1)
              t-2-elem-type (second t-2)]
          (get-bindings t-1-elem-type t-2-elem-type bindings)))))