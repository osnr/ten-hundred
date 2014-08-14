(ns ten-hundred.levels.definition
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [ten-hundred.terms :as terms]
            [ten-hundred.drag :as drag]))

(defn empty-definition []
  {:term ""
   :meaning ""})

(defn handle-term-change! [e definition]
  (om/update! definition :term (string/replace (.. e -target -value) #" " "_")))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll-top (.-scrollTop textarea))
    (om/set-state! owner :scroll-width (.-scrollWidth textarea))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn handle-focus! [e path control]
  (put! control [:author path]))

(defn definition-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [path
                                author-path drag-clone
                                terms
                                control
                                scroll-top scroll-width]}]
      (dom/div #js {:className "definitionWrapper"
                    :style (drag/drag-clone-style drag-clone)

                    :onMouseDown #(drag/drag-start! control :definition @definition path %)}
        (dom/div #js {:className (str "definition"
                                    (when (= author-path path)
                                      " authoring"))
                      :id (str "definition_" (string/join "_" path))}
          (dom/input #js {:type "text" :placeholder "Term"
                          :className "term"
                          :value (:term definition)
                          :onChange #(handle-term-change! % definition)
                          :onFocus #(handle-focus! % path control)})
          (dom/button #js {:className "remove"
                           :onClick #(put! control [:delete-definition path])}
                      (dom/i #js {:className "fa fa-times"}))
          (dom/div #js {:className "meaning"}
            (dom/textarea #js {:className "edit"
                               :value (:meaning definition)
                               :scrollTop scroll-top
                               :onScroll #(handle-scroll! % owner)
                               :onChange #(handle-meaning-change! % owner definition)
                               :onFocus #(handle-focus! % path control)})
            (apply dom/pre #js {:className "bg"
                                :ref "bg"
                                :scrollTop scroll-top
                                :style #js {:maxWidth scroll-width}}
              (terms/word-map #(terms/colorize-word terms %)
                              (:meaning definition)))))))))
