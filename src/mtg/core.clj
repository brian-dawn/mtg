(ns mtg.core
  (:require [cheshire.core :refer :all])
  (:gen-class))

(def p clojure.pprint/pprint)

(def cards (map second (parse-string (slurp "AllCards-x.json") true)))

(defn -main []
  ;; Run this like normally, or run this line from a REPL.
  (p (map :name (take 10 (sort-by :name cards))))


  )

