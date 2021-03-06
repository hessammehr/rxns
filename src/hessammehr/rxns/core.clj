(ns hessammehr.rxns.core
  (:require [clojure.string :as string]))


(def periodic-table {:C 12.011 :H 1.0079 :N 14.007 :O 15.999
                     :P 30.974 :Cl 35.453})

(defn parse-fragment [fragment]
  (let [element (re-find #"^\p{Upper}\p{Lower}*" fragment)
        count (re-find #"\d+$" fragment)]
    {:element (keyword element) :count (if count (read-string count) 1)}))

(defn parse-chemical [formula]
  (let [fragments (re-seq #"\p{Upper}\p{Lower}*\d*" formula)]
    {:composition (map parse-fragment fragments)
     :label (clojure.string/trim formula)}))

(defn parse-chemical-with-coefficient [term]
  (let [term (string/trim term)
        [_ coefficient formula] (re-find #"(\d*)(.+)" term)]
    (assoc (parse-chemical formula)
      :coefficient (if (seq coefficient) (read-string coefficient) 1))))

(defn parse-reaction-side [side]
  (->> (string/split side #"\+")
       (map parse-chemical-with-coefficient)))

(defn parse-reaction [rxn]
  (let [sides (rest (re-find #"([^=>]+)=>([^=>]+)" rxn))]
    (println sides)
    (->> sides
         (map parse-reaction-side)
         (zipmap [:reactants :products]))))

(defn flat [rxn]
  (concat (:reactants rxn) (:products rxn)))

(defn get-rxn-component
  "Reaction: 2H2 + O2 => 2H2O"
  [rxn item &{:keys [role]}]
  (let [items (case role
                :reactants (:reactants rxn)
                :reactant (:reactants rxn)
                :products (:products rxn)
                :product (:products rxn)
                (flat rxn))]
    (if (number? item)
      (nth items item)
      (first(filter #(= item (:label %)) items)))))

(defn dump-fragment [fragment]
  (str (name (:element fragment)) (if (> (:count fragment) 1) (:count fragment) "")))

(defn dump-chemical [chemical]
  (->> (for [fragment (:composition chemical)]
        (dump-fragment fragment))
       (apply str)))

(defn dump-chemical-with-coefficient [chemical]
  (str (if (> (:coefficient chemical) 1) (:coefficient chemical) "")
       (dump-chemical chemical)))

(defn dump-reaction-side [side]
  (->> side
       (map dump-chemical-with-coefficient)
       (interpose " + ")
       (apply str)))

(defn dump-reaction [rxn]
  (let [reactants (dump-reaction-side (:reactants rxn))
        products (dump-reaction-side (:products rxn))]
    (str reactants " => " products)))

(defn fragment-mass [fragment]
  (* (->> fragment :element periodic-table) (:count fragment)))

(defn mass [chemical]
  (let [composition (:composition chemical)]
    (->> composition
         (map fragment-mass)
         (reduce +))))


(defn parse-amount [chemical & [{:keys [g mL mol d M]}]]
  (let [MW (mass chemical)]
    (cond mol mol
          (and g MW) (/ g MW)
          (and mL M) (/ (* mL MW) 1000)
          (and mL d) (/ (* mL d) MW))))


(defn stoichiometry [rxn item amount & {:keys [role]}]
  (let [chemical (get-rxn-component rxn item :role role)
        moles (parse-amount chemical amount)
        eq (/ moles (:coefficient chemical))]
    (for [x (flat rxn) :when (not= (:label x) item)
          :let [x-moles (* eq (:coefficient x))
                x-mass (mass x)
                x-amount (* x-moles x-mass)]]
      (assoc x :amount x-amount))
    ))


