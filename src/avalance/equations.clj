(ns ^{:doc "Generate expressions from pcfg and make executable"
      :author "Zenna Tavares"}
  avalance.equations)

(defn make-lambda
  [expr]
  (eval (list 'fn '[x] expr)))

(defn make-lambda-args
  "Make a function from an expression with some args"
  [expr args]
  (eval (list 'fn args expr)))

; Probabilstic context free grammar
(def compound-pcfg
  {:start 'F
   :rules
    {'F [ {:prod 'V :weight 10000}
          {:prod '(UF V) :weight 1.6}
          {:prod '(BF V V) :weight 3.4}]
      'UF [{:prod 'Math/sin :weight 1.2}
           {:prod 'Math/cos :weight 1.2}]
      'BF [{:prod '+   :weight 1.0}
           {:prod '-   :weight 1.0}
           {:prod '*   :weight 1.0}
           {:prod '/   :weight 1.0}]
      'V  [{:prod 'F   :weight 1.0}]}})

(defn add-rule-to-pcfg
  "Add variables to V production rule of grammar"
  [variable-prods pcfg non-term]
  (update-in pcfg [:rules non-term] (fn [productions] (concat productions variable-prods))))

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

(defn gen-expr-pcfg-arb
  "Generate an expression from a pcfg"
  [pcfg start-symb]
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
    (inner-loop start-symb)))

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