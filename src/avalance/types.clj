:type

; every type is a list where the first value is one of
; fun, list, tuple, type-var, prim
; the remaining values depend on the first value:
; fun: first value is another list where each elemenet is the argument
;      second value is a type of return argument
; 
(def reduce-t '(fun ((fun a b) (list a))
					 (list b)))

(defn types-match?
	[func value]
	)