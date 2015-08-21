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

(def un comp) ;; (un red?) sounds better than (comp red?)

;;######################
;;# Filter Functions
;;######################

(defn- color-test-builder
  "Builder for black? red? green? white? and blue?."
  [color]
  (fn [c] (not-nil? (some #(= color %) (:colors c)))))
(def black? [c] (color-test-builder "Black"))
(def red?   [c] (color-test-builder "Red"))
(def green? [c] (color-test-builder "Green"))
(def white? [c] (color-test-builder "White"))
(def blue?  [c] (color-test-builder "Blue"))
(def not-red?   (complement red?))
(def not-green? (complement green?))
(def not-white? (complement white?))
(def not-blue?  (complement blue?))
(def not-black? (complement black?))
(def colorless? (every-pred not-red? not-green? not-white? not-blue? not-black?))

(defn- cmc-test-builder
  [operator cmc]
  (fn [c]
    (let [card-cmc (:cmc c)]
      (if (nil? card-cmc)
        false
        (operator (:cmc c) cmc)))))
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
(def modern-legal?    (legal-builder "Modern"))
(def standard-legal?  (legal-builder "Standard"))
(def commander-legal? (legal-builder "Commander"))
(def legacy-legal?    (legal-builder "Legacy"))
(def vintage-legal?   (legal-builder "Vintage"))

(defn type-test-builder [type]
  (fn [c] (not-nil? (some #(= type %) (:types c)))))
(def artifact?     (type-test-builder "Artifact"))
(def creature?     (type-test-builder "Creature"))
(def land?         (type-test-builder "Land"))
(def enchantment?  (type-test-builder "Enchantment"))
(def planeswalker? (type-test-builder "Planeswalker"))
(def instant?      (type-test-builder "Instant"))
(def sorcery?      (type-test-builder "Sorcery"))
(def tribal?       (type-test-builder "Tribal"))

(defn has-text
  "Returns whether or not a card contains a regex expression. Ignores case."
  [text]
  (fn [c]
    (let [txt (:text c)]
      (if (nil? txt)
        false
        (not-nil? (re-find (re-pattern (clojure.string/lower-case text))
                           (clojure.string/lower-case (:text c))))))))

;; Useful compositions.
(def flample? (every-pred (has-text "flying") (has-text "trample") creature?))

;;######################
;;# Find Functions
;;######################

(defn choose [cards & predicates]
  (filter (apply every-pred predicates) cards))

(def pchoose (comp print-cards (partial choose cards)))

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

;; (pchoose (has-text "Suspend") (cmc> 9) red? creature?)

;; (not-nil? (re-find (re-pattern "Suspend") "faskfjSuspend"))

;; (pchoose (has-text "flying") (has-text "trample") (cmc> 2) red? creature?)

;; (first cards)

;; ((has-text "apply") (first cards))

(pchoose flample? not-red? blue?)
