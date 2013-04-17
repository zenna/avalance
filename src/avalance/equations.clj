(ns avalance.equations)

(defn frequency
	[period]
  (/ 1 period))

(defn energy
  [mass]
  (* mass (* 3E8 3E8)))

(defn e1
  [p1]
  (+ 10 (* p1 p1)))

; Constant
(defn e2
  [p1]
  10)

; ok
(defn e3
  [p1]
  (+ (* p1 p1 p1 5) (* p1 p1 3) (* 9 p1)))

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

(defn make-lambda
  [expr]
  (eval (list 'fn '[x] expr)))

(defn make-lambda-args
  "Make a function from an expression with some args"
  [expr args]
  (eval (list 'fn args expr)))

; Example attribute
(defn x-in-denom?
  [expr]
  (let [pos-div (.indexOf (flatten expr) '/)]
    (cond
      (= pos-div -1) false
      (= 'x (nth (flatten expr) (+ 2 pos-div))) true
      :else false)))

; Probabilstic context free grammar
(def compound-pcfg
  {:start 'F
   :rules
    {'F [ {:prod 'V :weight 10}
          {:prod '(UF V) :weight 1.6}
          {:prod '(BF V V) :weight 3.4}]
      'UF [{:prod 'Math/sin :weight 1.2}
           {:prod 'Math/cos :weight 1.2}]
      'BF [{:prod '+   :weight 1.0}
           {:prod '-   :weight 1.0}
           {:prod '*   :weight 1.0}
           {:prod '/   :weight 1.0}]
      'V  [{:prod 'F   :weight 1.0}]}})

(defn add-data-to-pcfg
  "Add variables to V production rule of grammar"
  [variable-prods pcfg]
  (update-in pcfg [:rules 'V] (fn [productions] (concat productions variable-prods))))

(defn sample-production
  "Probabalistically sample a production"
  [productions]
  (let [sorted-prods (sort-by :weight > productions)
        total-weight (apply + (map #(:weight %1) productions))
        rand-point (rand total-weight)]
      (loop [sorted-prods-loop sorted-prods accumulated-weight 0.0]
        (if (>= (+ accumulated-weight (:weight (first sorted-prods-loop))) rand-point)
          (first sorted-prods-loop)
          (recur (rest sorted-prods-loop) (+ accumulated-weight (:weight (first sorted-prods-loop))))))))

(defn gen-expr-pcfg
  "Generate an expression from a pcfg"
  [pcfg]
  (let [inner-loop (fn il [lhs-symb] 
      (cond
        ; Non-terminal? Probabalistically choose transition
        (contains? (:rules pcfg) lhs-symb)
          (let [productions ((:rules pcfg) lhs-symb)
                production (:prod (sample-production productions))]
            (if (list? production)
              (map il production)
              (il production)))

        ; Otherwise it's a terminal
        :else
          lhs-symb))]
    (inner-loop (:start pcfg))))

; Algorithm sorts weights onto a line and samples from the line
(defn sample-production
  "Probabalistically sample a production"
  [productions]
  (let [sorted-prods (sort-by :weight > productions)
        total-weight (apply + (map #(:weight %1) productions))
        rand-point (rand total-weight)]
      (loop [sorted-prods-loop sorted-prods accumulated-weight 0.0]
        (if (>= (+ accumulated-weight (:weight (first sorted-prods-loop))) rand-point)
          (first sorted-prods-loop)
          (recur (rest sorted-prods-loop) (+ accumulated-weight (:weight (first sorted-prods-loop))))))))