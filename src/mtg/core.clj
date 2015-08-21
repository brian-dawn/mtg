(ns mtg.core
  (:require [cheshire.core :refer :all]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


(def p clojure.pprint/pprint)

(def cards (sort-by :name (map second (parse-string (slurp "AllCards-x.json") true))))

;;######################
;;# Generic Helper Functions
;;######################

(def not-nil? (complement nil?))

(defn format-text [text] (clojure.string/replace (str "\n" text) "\n" "\n\t"))

(defn empty-string-if-nil [s]
  (if (nil? s)
    ""
    s))

(defn print-card [c] (println (:name c) (:types c) (:manaCost c)
                              (str (empty-string-if-nil (:power c))
                                   (if (:power c) "/" "")
                                   (empty-string-if-nil (:toughness c)))
                               (format-text (:text c)) "\n"))

(defn print-cards [cs] (doseq [c cs] (print-card c)))

(def un complement) ;; (un red?) sounds better than (complement red?)


;;######################
;;# Attribute Readers
;;######################
(defn parse-mtg-numeric
  "Coerce '1' to 1
  '*' to 0
  1  to 1
          nil to 0"
  [s]
  (try (int (bigdec s))
       (catch Exception e
         0)))

(defn power     [c] (parse-mtg-numeric (:power c)))
(defn toughness [c] (parse-mtg-numeric (:toughness c)))
(def cmc :cmc)
(def name :name)
(def mana-cost :manaCost)

;;######################
;;# Filter Functions
;;######################

(defn- color-test-builder
  "Builder for black? red? green? white? and blue?."
  [color]
  (fn [c] (not-nil? (some #(= color %) (:colors c)))))
(def black? (color-test-builder "Black"))
(def red?   (color-test-builder "Red"))
(def green? (color-test-builder "Green"))
(def white? (color-test-builder "White"))
(def blue?  (color-test-builder "Blue"))

(def not-red?   (complement red?))
(def not-green? (complement green?))
(def not-white? (complement white?))
(def not-blue?  (complement blue?))
(def not-black? (complement black?))

(def colorless? (every-pred not-red? not-green? not-white? not-blue? not-black?))

(def only-black? (every-pred not-red? not-green? not-white? not-blue? black?))
(def only-red?   (every-pred red?     not-green? not-white? not-blue? not-black?))
(def only-green? (every-pred not-red? green?     not-white? not-blue? not-black?))
(def only-white? (every-pred not-red? not-green? white?     not-blue? not-black?))
(def only-blue?  (every-pred not-red? not-green? not-white? blue?     not-black?))

(defn- optional-operator-test-builder
  "Used for the creation of comparison functions for numeric values."
  [key operator val]
  (fn [c]
    (let [card-val (key c)]
      (if (nil? card-val)
        false
        (operator (parse-mtg-numeric card-val) val)))))
(def cmc<  (partial optional-operator-test-builder :cmc <))
(def cmc<= (partial optional-operator-test-builder :cmc <=))
(def cmc=  (partial optional-operator-test-builder :cmc =))
(def cmc>  (partial optional-operator-test-builder :cmc >))
(def cmc>= (partial optional-operator-test-builder :cmc >=))

(def power<  (partial optional-operator-test-builder :power <))
(def power<= (partial optional-operator-test-builder :power <=))
(def power=  (partial optional-operator-test-builder :power =))
(def power>  (partial optional-operator-test-builder :power >))
(def power>= (partial optional-operator-test-builder :power >=))

(def toughness<  (partial optional-operator-test-builder :toughness <))
(def toughness<= (partial optional-operator-test-builder :toughness <=))
(def toughness=  (partial optional-operator-test-builder :toughness =))
(def toughness>  (partial optional-operator-test-builder :toughness >))
(def toughness>= (partial optional-operator-test-builder :toughness >=))

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

;; (pchoose flample? (un creature?))

;; (pchoose flample? creature? black? not-red? not-green? not-white? not-blue?)

;; (print-cards (sort-by :cmc (choose cards flample? creature? black? not-red? not-green? not-white? not-blue?)))

;; (pchoose (cmc> 5) (power< 4) flample?)

;; (cmc (second (choose cards creature?)))
