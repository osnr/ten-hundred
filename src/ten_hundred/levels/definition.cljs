(ns ten-hundred.levels.definition
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]
            [clojure.string :as string]

            [ten-hundred.terms :as terms]
            [ten-hundred.drag :as drag]
            [ten-hundred.edit :as edit]))

(defn empty-definition []
  {:term ""
   :meaning ""})

(defn handle-term-change! [e definition]
  (om/update! definition :term (terms/escape-term (.. e -target -value))))

(defn handle-focus! [e path control]
  (put! control [:author path]))

(defcomponent definition-view [definition owner]
  (render-state [this {:keys [terms
                              highlight
                              path
                              author-path drag-clone
                              scroll-top scroll-width]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)]
      (dom/div {:class "definitionWrapper"
                :style (drag/drag-clone-style drag-clone)

                :on-mouse-down
                (when-not read-only
                  #(drag/drag-start! control :definition @definition path %))}

        (dom/div {:class (str "definition"
                              (when (= author-path path)
                                " authoring"))
                  :id (str "definition_" (string/join "_" path))}

          (dom/input {:type "text" :placeholder "Term"
                      :class "term"
                      :value (:term definition)
                      :on-change (when-not read-only
                                   #(handle-term-change! % definition))
                      :on-focus #(handle-focus! % path control)})
          (when-not read-only
            (dom/button {:class "remove"
                         :on-click #(put! control [:delete-definition path])}
                        (dom/i {:class "fa fa-times"})))

          (dom/div {:class "meaning"}
            (om/build edit/editor-view definition
                      {:state {:terms terms
                               :highlight highlight

                               :path path}})))))))
