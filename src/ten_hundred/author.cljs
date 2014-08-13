(ns ten-hundred.author
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [ten-hundred.terms :as terms]
            [ten-hundred.dict :as dict]
            [goog.dom.classlist :as classlist]))

(defn hover-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [terms term left bottom]}]
      (dom/div #js {:className "hover"
                    :style #js {:position "fixed"
                                :left left
                                :bottom bottom}}
               (:meaning (terms/find-term terms (string/lower-case term)))))))

; expansion view
(declare expand-word)
(defn expand [terms expand-to meaning]
  (terms/word-map #(expand-word terms expand-to %) meaning))

(defn expand-word [terms expand-to word]
  (let [lc-word (string/lower-case word)]
    (if-let [{[term-level-idx _] :path
              term-meaning :meaning}
             (terms/find-term terms lc-word)]
      (if (>= term-level-idx expand-to)
        (apply dom/span #js {:className "expanded-word"}
               "["
               (conj (expand terms expand-to term-meaning)
                     "]"))
        (dom/span #js {:className "expandable-word"} word))

      (if (dict/words lc-word)
        word
        (dom/span #js {:className "notDefined"} word)))))

(defn handle-expand-to-change! [e owner]
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll-top (.-scrollTop textarea))
    (om/set-state! owner :scroll-width (.-scrollWidth textarea))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn handle-mousemove! [e owner definition terms]
  (let [target (.-target e)]
    (cond (classlist/contains target "defined")
          (when (not (om/get-state owner :hover-state))
            (let [bounds (.getBoundingClientRect target)]
              (om/set-state! owner :hover-state
                             {:terms terms
                              :term (.-innerText target)

                              :left (.-left bounds)
                              :bottom
                              (- (.-innerHeight js/window)
                                 (.-top bounds))

                              :alpha false})))

          :else
          (om/set-state! owner :hover-state nil))))

(defn handle-bg-click! [e owner control]
  (let [target (.-target e)]
    (cond (classlist/contains target "notDefined") ; not defined term
          (put! control [:define (.-innerText target)])

          (classlist/contains target "defined")
          (let [level-idx (js/parseInt (.-levelIdx (.-dataset target)))
                definition-idx (js/parseInt (.-definitionIdx (.-dataset target)))]
            (put! control [:author [level-idx definition-idx]])))))

(defn author-view [definition owner]
  (reify
    om/IWillReceiveProps
    (will-receive-props [this next-props]
      (let [prev-props (om/get-props owner)]
        (when (not= (:term prev-props) (:term next-props))
          (om/set-state! owner :expand-to nil))))

    om/IRenderState
    (render-state [this {:keys [control level-idx expand-to close terms
                                hover-state
                                scroll-top scroll-width]}]
      (let [expand-to (or expand-to level-idx)]
        (dom/div #js {:className "author"}
          (dom/div #js {:className "authorContentWrapper"}
            (dom/input #js {:className "authorTerm"
                            :type "text" :placeholder "Term"
                            :value (:term definition)
                            :onChange #(om/update! definition :term (string/replace (.. % -target -value) #" " "_"))})
              (dom/div #js {:className "authorContent"}
                (apply dom/div #js {:className "authorMeaning"
                                    :onMouseMove #(handle-mousemove! % owner definition terms)}
                  (if (= expand-to level-idx)
                    [(dom/textarea #js {:className "edit"
                                        :ref "edit"

                                        :scrollTop scroll-top

                                        :value (:meaning definition)
                                        :onScroll #(handle-scroll! % owner)
                                        :onChange #(handle-meaning-change! % owner definition)})
                     (apply dom/pre #js {:className "bg"
                                         :ref "bg"

                                         :scrollTop scroll-top
                                         :style #js {:maxWidth scroll-width}

                                         :onClick #(handle-bg-click! % owner control)}
                            (terms/word-map #(terms/colorize-word terms %)
                                            (:meaning definition)))]
                    [(apply dom/div #js {:className "edit"}
                            (expand terms expand-to (:meaning definition)))]))

                (when hover-state
                  (om/build hover-view definition
                            {:state hover-state}))

                (dom/div #js {:className (str "expandControls"
                                              (when (= level-idx 0)
                                                " disabled"))}
                  (dom/i #js {:className "fa fa-plus-square"})
                  (dom/input #js {:className "expandSlider"
                                  :type "range"
                                  :disabled (= level-idx 0)
                                  :min 0
                                  :max level-idx
                                  :value (or expand-to level-idx)
                                  :onChange #(handle-expand-to-change! % owner)})
                  (dom/i #js {:className "fa fa-minus-square"})))))))))
