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

(defn format-text [text] (clojure.string/replace (str "\n" text) "\n" "\n\t"))

(defn print-card [c] (println (:name c) (:types c) (:manaCost c) (format-text (:text c)) "\n"))

(defn print-cards [cs] (doseq [c cs] (print-card c)))

;;######################
;;# Filter Functions
;;######################

(defn- color-test-builder
  "Builder for black? red? green? white? and blue?."
  [color]
  (fn [c] (not-nil? (some #(= color %) (:colors c)))))
(defn black? [c] ((color-test-builder "Black") c))
(defn red?   [c] ((color-test-builder "Red")   c))
(defn green? [c] ((color-test-builder "Green") c))
(defn white? [c] ((color-test-builder "White") c))
(defn blue?  [c] ((color-test-builder "Blue")  c))

(defn- cmc-test-builder
  [operator cmc]
  (fn [c]
    (let [card-cmc (:cmc c)]
      (if (nil? card-cmc)
        false
        (operator cmc (:cmc c))))))
(defn cmc<  [cmc] (cmc-test-builder <  cmc))
(defn cmc<= [cmc] (cmc-test-builder <= cmc))
(defn cmc=  [cmc] (cmc-test-builder =  cmc))
(defn cmc>  [cmc] (cmc-test-builder >  cmc))
(defn cmc>= [cmc] (cmc-test-builder >= cmc))

(defn- legal-builder [fmt]
  (fn [c]
    (= "Legal"
       (-> (filter #(= (:format %) fmt) (:legalities c))
           first
           :legality))))
(defn modern-legal?    [c] ((legal-builder "Modern")    c))
(defn standard-legal?  [c] ((legal-builder "Standard")  c))
(defn commander-legal? [c] ((legal-builder "Commander") c))
(defn legacy-legal?    [c] ((legal-builder "Legacy")    c))
(defn vintage-legal?   [c] ((legal-builder "Vintage")   c))

(defn type-test-builder [type]
  (fn [c] (not-nil? (some #(= type %) (:types c)))))
(defn artifact?     [c] ((type-test-builder "Artifact")     c))
(defn creature?     [c] ((type-test-builder "Creature")     c))
(defn land?         [c] ((type-test-builder "Land")         c))
(defn enchantment?  [c] ((type-test-builder "Enchantment")  c))
(defn planeswalker? [c] ((type-test-builder "Planeswalker") c))
(defn instant?      [c] ((type-test-builder "Instant")      c))
(defn sorcery?      [c] ((type-test-builder "Sorcery")      c))
(defn tribal?       [c] ((type-test-builder "Tribal")       c))

(defn choose [cards & predicates]
  (filter (apply every-pred predicates) cards))

;;######################
;;# Find Functions
;;######################

(defn find-by-name [cs name] (first (filter #(= (:name %) name) cs)))

;;######################
;;# Examples
;;######################

;; ((cmc< 3) (first cards))

;; (p (map :name (take 10 (filter (cmc= 3) cards))))

;; (p (red? (first cards)))

;; (red? (first cards))

;; (print-cards (take 10 (sort-by :name (filter red? cards))))

;; (filter #(= (:name %) "Ghostfire") (filter red? cards))


;; (filter #(= (:name %) "Ghostfire") cards)

;; ;; (p (map :name (take 10 (sort-by :name cards))))

;; (standard-legal? (find-by-name cards "Ghostfire"))

;; (artifact? (find-by-name cards "Black Lotus"))

;; (planeswalker? (find-by-name cards "Liliana of the Veil"))

;; (print-cards (filter planeswalker? cards))

;; (print-cards (choose cards (cmc> 4) red? creature?))

(print-cards (choose cards (cmc< 8) red? creature?))
