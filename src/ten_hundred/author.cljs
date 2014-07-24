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

(defn handle-expand-to-change! [e owner]
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll-top (.-scrollTop textarea))
    (set! (.-scrollTop bg) (.-scrollTop textarea))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn handle-bg-click! [e owner control]
  (js/console.log (.-id (.-target e))))

(defn author-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [control level-idx expand-to close terms]}]
      (dom/div #js {:className "author"}
        (dom/div #js {:className "authorContentWrapper"}
          (dom/div #js {:className "authorContent"}
            (apply dom/div #js {:className "authorMeaning"}
              (if (= expand-to level-idx)
                [(dom/textarea #js {:className "edit"
                                    :ref "edit"
                                    :value (:meaning definition)
                                    :onScroll #(handle-scroll! % owner)
                                    :onChange #(handle-meaning-change! % owner definition)})
                 (apply dom/pre #js {:className "bg"
                                     :ref "bg"
                                     :onClick #(handle-bg-click! % owner control)}
                        (terms/word-map #(terms/colorize-word terms %)
                                        (:meaning definition)))]
                [(apply dom/div #js {:className "edit"}
                        (expand terms expand-to (:meaning definition)))]))
            (dom/input #js {:className "expandSlider"
                            :type "range"
                            :disabled (= level-idx 0)
                            :min 0
                            :max level-idx
                            :value expand-to
                            :onChange #(handle-expand-to-change! % owner)})))))))
