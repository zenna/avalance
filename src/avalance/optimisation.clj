(ns avalance.optimsation)

(defn nelder-mead-random-restart
  [parameters cost-func num-iters]
  "Use nelder-mead with random init"
  (repeatedly)
  (nelder-mead parameters cost-func (repeatedly (count parameters) rand )))

(defn init-simplex
  "Initialise a simplex anchored on a point"
  [point step-size]
  (let [degen-simplex (repeat (inc (count parameters)) init-point)]
    (map (fn [point] ) degen-simplex)))

(defn nelder-mead
  [parameters cost-func init-point]
  "Perform parameter optimsation with nelder-mead"
  (let [step-size 10]
    (loop [simplex (init-simplex init-point step-size)]
      ; Find best points
      (let [costs (map cost-func simplex)
            (sort-by 'cost costs)
            ]))) 