(ns ten-hundred.author
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
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defn handle-meaning-change [e owner definition]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (js/window.setTimeout 
     (fn [_]
       (let [height (str (.-scrollHeight textarea) "px")]
         (set! (.-height (.-style textarea)) height)
         (set! (.-height (.-style bg)) height)))
     0)
    (om/update! definition :meaning (.-value textarea))))

(defn author-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [control level-idx expand-to close terms] :as state}]
      (println level-idx expand-to (= level-idx expand-to))
      (dom/div #js {:className "author"
                    :onClick (constantly false)}
        (dom/div #js {:className "authorContent"}
          (dom/input #js {:type "range"
                          :min 0
                          :max level-idx
                          :value expand-to
                          :onChange #(handle-expand-to-change % owner)})
          (apply dom/div #js {:className "authorMeaning"}
            (if (= expand-to level-idx)
              [(dom/textarea #js {:className "edit"
                                  :value (:meaning definition)
                                  :onChange #(handle-meaning-change % owner definition)})
               (apply dom/pre #js {:className "bg"
                                   :ref "bg"}
                      (terms/word-map #(terms/colorize-word terms control %)
                                      (:meaning definition)))]
              (expand terms expand-to (:meaning definition)))))))))
