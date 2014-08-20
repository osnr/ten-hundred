(ns ten-hundred.levels.definition
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]
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

(defcomponent definition-view [definition owner]
  (render-state [this {:keys [path
                              author-path drag-clone
                              terms
                              control
                              scroll-top scroll-width]}]
    (dom/div {:class "definitionWrapper"
              :style (drag/drag-clone-style drag-clone)

              :on-mouse-down #(drag/drag-start! control :definition @definition path %)}

      (dom/div {:class (str "definition"
                                (when (= author-path path)
                                  " authoring"))
                :id (str "definition_" (string/join "_" path))}

        (dom/input {:type "text" :placeholder "Term"
                    :class "term"
                    :value (:term definition)
                    :on-change #(handle-term-change! % definition)
                    :on-focus #(handle-focus! % path control)})
        (dom/button {:class "remove"
                     :on-click #(put! control [:delete-definition path])}
                    (dom/i {:class "fa fa-times"}))

        (dom/div {:class "meaning"}
          (dom/textarea {:class "edit"
                         :value (:meaning definition)
                         :scroll-top scroll-top
                         :on-scroll #(handle-scroll! % owner)
                         :on-change #(handle-meaning-change! % owner definition)
                         :on-focus #(handle-focus! % path control)})
          (dom/pre {:class "bg"
                    :ref "bg"
                    :scroll-top scroll-top
                    :style {:max-width scroll-width}}
            (terms/word-map #(terms/colorize-word terms %)
                            terms/colorize-tex
                            (:meaning definition))))))))
