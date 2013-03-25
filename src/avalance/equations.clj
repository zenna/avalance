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

(defn random-function 
  []
  (rand-nth '(+ * /)))
 
(defn random-terminal
  []
  (rand-nth (list 'x (rand 10))))
 
(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (list (random-function)
          (random-code (dec depth))
          (random-code (dec depth)))))

(defn random-code-custom-terminals
  [depth gen-terminal]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (gen-terminal)
    (list (random-function)
          (random-code-custom-terminals (dec depth) gen-terminal)
          (random-code-custom-terminals (dec depth) gen-terminal))))

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

; Generate num functions which adhere to attribute constraints
(defn gen-funcs
  "Generate num functions which adhere to attribute constraints"
  [attr number]
  (loop [funcs []]
    (if (= (count funcs) number)
      funcs
      (let [expr (random-code 2)
            printed (if (attr expr) (println expr))]
        (if (attr expr)
          (recur (concat funcs [(make-lambda expr)])) 
          (recur funcs))))))

; Example attribute
(defn x-in-denom?
  [expr]
  (let [pos-div (.indexOf (flatten expr) '/)]
    (cond
      (= pos-div -1) false
      (= 'x (nth (flatten expr) (+ 2 pos-div))) true
      :else false)))