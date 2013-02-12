; A set of program transforms

; Replace random expression
; Find a random expression
; Find its type
; try n times to find another one in the library

1. COUNT ELEMENTS FLATTEN
2. gET type
3. FIND OBJECT
IF REPLACEMENT OBJECT HAS DEPENDENCIES RECURSE

PROBLEMS:
FINDING PLACE OF CHANGE
STOPPING INFINITE RECURSION

(defn replace-in-list [coll n x]
  (concat (take n coll) (list x) (nthnext coll (inc n))))

(defn replace-in-sublist [coll ns x]
  (if (seq ns)
    (let [sublist (nth coll (first ns))]
      (replace-in-list coll
                       (first ns)
                       (replace-in-sublist sublist (rest ns) x)))
    x))

(defn change-random-branch
	[func lib]
	(let [flat-func (flatten func)
		  rand-elem-pos lib]
		  (loop [func-section func pos-vec [] i 0 num-to-go rand-elem-pos]
		  	(if (zero? num-to-go)
		  		(conj pos-vec i)
			  	(if (list? (first func-section))
			  		(recur (rest func-section) (conj pos-vec i) 0 (dec num-to-go))
			  		(recur (rest func-section) pos-vec (inc i) (dec num-to-go)))))))


Goal 4

(1 2 3 (1 2) 3)
0 1 2 3! 0!


