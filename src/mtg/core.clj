(ns mtg.core
  (:require [cheshire.core :refer :all]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


(def p clojure.pprint/pprint)

(def cards (map second (parse-string (slurp "AllCards-x.json") true)))

;; Helper functions.



;; (p (map :name (take 10 (sort-by :name cards))))

