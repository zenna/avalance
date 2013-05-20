(ns ^{:doc "A grammar based on pattern matching."
      :author "Zenna Tavares"}
  avalance.grammar
  (:use clozen.helpers)
  (:use avalance.equations))

(defn pattern-evaluator
  "Evaluate a pattern on an expression, return matching variables
  and their"
  [expr ptrn]
  (let [matches (coll-to-keys expr
                        (fn [elem pos] (zero? pos)))]
    (map (fn [match] {'?match match}) (seq matches))))

; FIXME, I NEED ANOTHER LEVEL OF CONTROL FOR MULTIPL BRANCHES
(defn extend-expr
  "Extend the expression with a pattern grammar"
  [expr pmg]
  (let [matches (map #(pattern-evaluator expr %)
                      (keys (:pattern-rules pmg)))
        ; pvar (println "Matches" matches)
        {ptrn :ptrn match :match} ((:selector pmg) matches)
        ; pvar (println "Pattern" ptrn "match" match)
        ; {:prod variable :weight 10000.0}
        ext-pmg (pass
                    #(add-rule-to-pcfg 
                      [{:prod (second (second %1))
                       :weight 1.0}]
                      %2
                      (first %1))
                    pmg
                    (seq match))
        merged-pmg (assoc ext-pmg :rules
                                    (merge (:rules ext-pmg)
                                            (:pattern-rules pmg)))
        ; pvar (println "EXTENDED pmg" merged-pmg)
        ; FIXME: SAME AS ABOVE
        extended (gen-expr-pcfg-arb merged-pmg ptrn)
        ; pvar (println "extended" extended)
        final-expr (pass #(replace-in-sublist %2 (first (second %1)) extended)
                          expr
                          (seq match))]
        final-expr))