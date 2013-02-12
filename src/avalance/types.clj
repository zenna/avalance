(ns avalance.types)

; 3. do consistency mathign in func-matchaes-args
; 4. Write transformation 

if
1-9
"string"
and

; list functions
first
second
last
concat
conj
list


; bool
=
not=
and

; higher order
map
reduce

sym type eval deref
fn -> Symbol -> error -> clojure.core.fn
let -> Symbol-> error -> clojure.core.$let
[x y z] -> clojure.lang.persistentlist -> error -> error
make-step, if -> symbol -> error -> error
:typesig -> keyword
fn* -> symbol -> error -> error
p1__1215# -> symbol -> error -> error


(def map-t '(Fun ((Fun ((Tv a)) (Tv b))
   (List (Tv a))) 
  (List (Tv a))))

(def inc-t '(Fun ((Num)) (Num)))

(def list-int-t '(List (Num)))

(def num-t '(Num))

(def map-t '(Fun ((Fun ((Tv a)) (Tv b))
   (List (Tv a))) 
  (List (Tv a))))

(def bool-t '(Bool))

(def types {'map map-t 'inc inc-t
            java.lang.Boolean bool-t})

; check if its in a 
; otherwise check if its a standard type, e.g. a string
; otherwise fail
(defn get-type
  [symb]
  (let [cust-type (find types symb)
        symb-type (type symb)]
    (cond
      (not= cust-type nil)
        cust-type
      (= symb-type clojure.lang.Symbol)
        (ok)
      (= symb-type clojure.lang.PersistentVector)
        (ok)
      :else
        (throw (Throwable. "Symbol type not found")))))

; every type is a list where the first value is one of
; fun, list, tuple, type-var, prim
; the remaining values depend on the first value:
; fu(throw (Throwable. "Some text"))n: first value is another list where each elemenet is the argument
;      second value is a type of return argument 
(defn combine_bindings
  [binding-1 binding-2]
  (let [types-match? (and (first binding-1) (first binding-2))
        bindings (vec (concat (second binding-1) (second binding-2)))]
        [types-match? bindings]))

(declare same-type?)

(defn get-bindings
  "Compare a list of arguments to a function and a list of arg types"
  [func-t args-t bindings]
  ; Assune funcs-t and args-t are same size, should check for this
  (if (= (count func-t) 1)
    (same-type? (first func-t) (first args-t) bindings)
    (recur (rest func-t)
           (rest args-t)
           (same-type? (first func-t) (first args-t) bindings))))

; Look 
(defn bindings-consistent?
  "Scan bindings to check they are all consistent"
  [bindings]
  true)

(defn func-matches-args?
  "Is a function type-consistent with a set of arguments"
  [func-t args-t]
  (let [func-args-t (second func-t)
        empty-bindings [true []]
        bindings (get-bindings func-args-t args-t empty-bindings)]
        (bindings-consistent? bindings)))

(defn same-type?
  [t-1 t-2 bindings]
  ""
  (let [t-1-class (first t-1)
        t-2-class (first t-2)
        failed_binding [false []]]
    (cond
      ; If one is a typevar but the other isn't
      ; e.g (tv a) (num), we bind the a to the num
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
          (same-type? t-1-elem-type t-2-elem-type bindings))

      ; Else tyoes must be fine and no tvs present
      :else bindings)))


(defn -main
  []
  ; (func-matches-args? map-t (list inc-t list-int-t)))
  (func-matches-args? inc-t (list num-t)))


(require 'avalance.types)
(use 'clojure.tools.trace)
(trace-ns 'avalance.types)
(-main)