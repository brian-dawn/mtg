(ns mtg.core
  (:require [cheshire.core :refer :all]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


(def p clojure.pprint/pprint)

(def cards (map second (parse-string (slurp "AllCards-x.json") true)))

;;######################
;;# Generic Helper Functions
;;######################
(def not-nil? (complement nil?))

;;######################
;;# Filter Functions
;;######################

(defn- color-test-builder
  "Builder for black? red? green? white? and blue?."
  [color]
  (fn [c] (not-nil? (some #(= color %) (:colors c)))))
(defn black? [c] ((color-test-builder "Black") c))
(defn red?   [c] ((color-test-builder "Red") c))
(defn green? [c] ((color-test-builder "Green") c))
(defn white? [c] ((color-test-builder "White") c))
(defn blue?  [c] ((color-test-builder "Blue") c))

(defn- cmc-test-builder
  [operator cmc]
  (fn [c] (operator cmc (:cmc c))))

(defn cmc<  [cmc] (cmc-test-builder <  cmc))
(defn cmc<= [cmc] (cmc-test-builder <= cmc))
(defn cmc=  [cmc] (cmc-test-builder =  cmc))
(defn cmc>  [cmc] (cmc-test-builder >  cmc))
(defn cmc>= [cmc] (cmc-test-builder >= cmc))

;;######################
;;# Find Functions
;;######################

(defn find-by-name [cs name] (first (filter #(= (:name %) name) cs)))

;;######################
;;# Examples
;;######################

((cmc< 3) (first cards))

(take 10 (filter (cmc< 3) (take 10 cards)))

(p (red? (first cards)))

(red? (first cards))

(p (map :name (take 10 (sort-by :name (filter red? cards)))))

(filter #(= (:name %) "Ghostfire") (filter red? cards))


(filter #(= (:name %) "Ghostfire") cards)

;; (p (map :name (take 10 (sort-by :name cards))))

(p (find-by-name cards "Ghostfire"))

