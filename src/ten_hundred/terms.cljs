(ns ten-hundred.terms
  (:require [om-tools.dom :as dom :include-macros true]
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

(defn find-term [terms token]
  (last (filter #(= (:term %) token) terms)))

(defn colorize-word [terms word]
  (let [lc-word (string/lower-case word)
        term-state (when terms (find-term terms lc-word))]
    (cond term-state
          (let [[level-idx definition-idx] (:path term-state)]
            (dom/span {:class "defined"

                       :data-level-idx level-idx
                       :data-definition-idx definition-idx}
                      word))

          (dict/words lc-word) word

          :else (dom/span {:class "notDefined"} word))))

(defn word-map [f text]
  (->> text
       (js/THParser.parse)
       (mapv (fn [[kind text]]
               (case kind
                 "spacing" text
                 "tex" (str "$$" text "$$")
                 "word" (f text))))))
