(ns ten-hundred.expansion
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [ten-hundred.terms :as terms]))

; definition expansion view
; 0 -> no expansion
; 1 -> 1 level deep
; etc
(declare expand-word)
(defn expand [terms expand-to meaning]
  (terms/word-map #(expand-word terms expand-to %) meaning))

(defn expand-word [terms expand-to word]
  (if-let [{[term-level-idx _] :path
            term-meaning :meaning}
           (terms/find-term terms (string/lower-case word))]
    (if (>= term-level-idx expand-to)
      (apply dom/span #js {:className "expanded-word"}
             "["
             (conj (expand terms expand-to term-meaning)
                   "]"))
      (dom/span #js {:className "expandable-word"} word))
    word))

(defn handle-expand-to-change [e owner]
  (om/set-state! owner :expand-to (.. e -target -value)))

(defn definition-expansion-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [level-idx expand-to close terms]}]
      (dom/div #js {:className "expansion"
                    :onClick (constantly false)}
        (dom/input #js {:type "range"
                        :min 0
                        :max level-idx
                        :value expand-to
                        :onChange #(handle-expand-to-change % owner)})
        (apply dom/div #js {:className "expanded-meaning"}
          (expand terms expand-to (:meaning definition)))
        (dom/button #js {:className "close"
                         :onClick close}
                    "x")))))
