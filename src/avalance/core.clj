(ns ^{:doc "Symbolic Regression main."
      :author "Zenna Tavares"}
  avalance.core
  (:use avalance.model))

(def data (gen-data-uniform line 10 100.0 5))
(println "Data Is" data)

(defn -main
  []
  (let [sols (new-find-expr data models [] all-attrs)]
    (println "\n\nThe unformatted solutions are:" sols)
    (println "\n\nThe formatted solutions are:" (map compile-expr sols))))