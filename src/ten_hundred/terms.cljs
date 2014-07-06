(ns ten-hundred.terms
  (:require [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [ten-hundred.dict :as dict]))

(defn find-terms [levels]
  (apply concat
    (map-indexed
     (fn [level-idx level]
       (->> level
            (map-indexed (fn [definition-idx {:keys [term meaning]}]
                           (hash-map :term (string/lower-case term)
                                     :meaning meaning
                                     :path [level-idx definition-idx])))
            (remove #(empty? (:term %)))))
     levels)))

(defn handle-term-click [control term-state]
  (put! control [:focus (:path term-state)]))

(defn find-term [terms token]
  (last (filter #(= (:term %) token) terms)))

(defn colorize-word [terms control word]
  (let [lc-word (string/lower-case word)]
    (cond (and terms (find-term terms lc-word))
          (dom/span #js {:className "defined"
                         :onClick #(handle-term-click
                                    control
                                    (find-term terms lc-word))}
                    word)

          (dict/words lc-word) word

          :else (dom/span #js {:className "uncommon"} word))))

(defn word-map [f text]
  (->> text
       (re-seq #"[^\w']+|[\w']+")
       (mapv #(if (re-matches #"[^\w']+" %)
                %
                (f %)))))
