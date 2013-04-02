(ns avalance.neldermead)

;todo
; termination tests - start with just number of iterations
; convert model into cost function

(defn nelder-mead-random-restart
  [parameters cost-func num-iters]
  "Use nelder-mead with random init"
  (repeatedly)
  (nelder-mead parameters cost-func (repeatedly (count parameters) rand )))

; point [p1 p3 ... pn]
; e.g. point = [10 20]
(defn init-simplex
  "Initialise a simplex anchored on a point"
  [point step-size]
  ; first construct a degenerate simplex, with all points identical
  (loop [simplex [point] dim 0]
    (if (> (count simplex) (count point))
      simplex
      (recur (conj simplex (update-in point [dim] #(+ %1 step-size)))
        (inc dim)))))

(defn vec-f
  "Like merge-with but for vectors"
  [f v1 v2]
  (loop [index 0 merged-vec []]
    (if (= index (count v1))
      merged-vec
      (recur (inc index) (conj merged-vec (f (nth v1 index) (nth v2 index)))))))

; TODO TEST
(defn vec-scalar-fd
  "scalar f (e.g. multiply division etc) of vector"
  [f v scalar]
  (map #(f %1 scalar) v))

; cost-func 
(defn nelder-mead
  [cost-func init-point]
  "Perform parameter optimsation with nelder-mead"
  (let [step-size 10
        ;nelder-mead parameters
        alpha 1.0
        beta 0.5
        gamma 2.0
        delta 0.5
        init-simplex init-point step-size]
    (loop [simplex-costs (map (fn [vertex] {:vertex vertex :cost (cost-func vertex)}) init-simplex)
           num-iter 0]
      ; Ordering: find worst, second worst and best point
      ; simplex-costs [{:vertex [.2 .3 .4] :cost .35} {...} ...]
      ; Check termination conditions first
      (if
        (or (<= num-iter 5)
            false)
        simplex-costs
        (let [simplex-costs (sort-by :cost simplex-costs)
              best-vertex (nth simplex-costs 0)
              sec-best-vertex (nth simplex-costs 1)
              worst-point (last simplex-costs)
              worst-index (dec (count simplex-costs))

              ; find centroid of the best side — opposite worst vertex 
              ; c = 1/n * vector sum of vertices
              centroid
                ; either i can destructure vertex-costs, to extract vectors
                ; or i can use original simplex
                (vec-scalar-f *
                  (reduce #(vec-f + (:vertex %1) (:vertex %2)) (dissoc simplex-costs worst-index))
                  (/ 1 (dec (count simplex))))
              reflection-vertex
                (vec-f + centroid 
                  (vec-scalar-f * (vec-f - centroid worst-vertex) alpha))
              expansion-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - reflection-vertex centroid) gamma))
              out-contraction-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - reflection-vertex centroid) beta))
              in-contraction-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - worst-vertex centroid) beta))]
          
          ; Reflect: test if cost of reflection point is between best and second worst
          (cond
            (and (< (cost-func worst-vertex) (cost-func reflection-vertex))
                  (<= (cost-func reflection-vertex) (cost-func best-vertex))))
            ; if so swap worst point for reflection point, terminate and recurse 
            (recur (assoc simplex-costs worst-index
              {:vertex reflection-vertex :cost (cost-func reflection-vertex)}))

            ; Expand: If reflection point is better than best, compute expansion point
            (and (< (cost-func best-vertex) (cost-func reflection-vertex))
                 (< (cost-func reflection-vertex) (cost-func expansion-vertex)))
            
            (recur (assoc simplex-costs worst-index
              {:vertex expansion-vertex :cost (cost-func expansion-vertex)}))

            ; Contract Outside: if fs <= fr < fh
            (and (<= (cost-func sec-best-vertex) (cost-func reflection-vertex))
                 (< (cost-func reflection-vertex) (cost-func worst-vertex))
                 (<= (cost-func out-contraction-vertex) (cost-func reflection-vertex)))

            (recur (assoc simplex-costs worst-index
              {:vertex out-contraction-vertex :cost (cost-func out-contraction-vertex)}))

            ; Contract Outside: if fs <= fr < fh
            (and (>= (cost-func reflection-vertex) (cost-func worst-vertex)
                 (< (cost-func centroid) (cost-func in-contraction-vertex))))

            (recur (assoc simplex-costs worst-index
              {:vertex in-contraction-vertex :cost (cost-func in-contraction-vertex)}))

            ; Otherwise shrink!
            :else 
              (recur (conj
                (map (fn [vertex] (let [shrunk-vertex (vec-f + worst-vertex 
                         (vec-scalar-f * (vec-f - (:vertex vertex) worst-vertex) beta))]
                    {:vertex shrunk-vertex :cost (cost-func shrunk-vertex)}))
                  (dissoc simplex-costs worst-index))
                (nth simplex-costs worst-index))))))))

  (defn model-to-cost-func)

(require 'avalance.neldermead)
(use 'clojure.tools.trace)
(trace-ns 'avalance.neldermead)

(defn dummy-cost-func
  "Dummy cost fucntion for testing"
  [parameters]
  (reduce + parameters))

(nelder-mead dummy-cost-func [1.0 2.0 3.0])