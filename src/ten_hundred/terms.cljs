(ns ten-hundred.terms
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom.classlist :as classlist]

            [ten-hundred.dict :as dict]
            [ten-hundred.tex :as tex]))

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

(defn colorize-tex [tex]
  (dom/span {:class "math"}
    (dom/span {:class "delimiter"} "$$")
    tex
    (dom/span {:class "delimiter"} "$$")))

(defn word-map [word-f tex-f text]
  (->> text
       (js/THParser.parse)
       (mapv (fn [[kind text]]
               (case kind
                 "spacing" text
                 "tex" (tex-f text)
                 "word" (word-f text))))))

(defn word-click! [control target]
  (cond (classlist/contains target "notDefined") ; not defined term
        (put! control [:define (.-innerText target)])

        (classlist/contains target "defined")
        (let [level-idx (js/parseInt (.-levelIdx (.-dataset target)))
              definition-idx (js/parseInt (.-definitionIdx (.-dataset target)))]
          (put! control [:author [level-idx definition-idx]]))))
