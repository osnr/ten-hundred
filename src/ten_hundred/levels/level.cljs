(ns ten-hundred.levels.level
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]
            [ten-hundred.util :refer [insert]]
            [ten-hundred.drag :as drag]
            [ten-hundred.levels.definition :as definition]))

(defn placeholder-definition-element []
  (dom/div {:class "placeholderDefinition"}))

(defn add-definition! [level]
  (om/transact! level #(conj % (definition/empty-definition))))

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup")))
(defcomponent level-view [level owner]
  (render-state [this {:keys [terms
                              level-idx
                              drag-clone author-path
                              drag-target-definition-idx]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)]
      (dom/div {:class "level"
                :id (str "level_" level-idx)
                :style (drag/drag-clone-style drag-clone)

                :on-mouse-down
                (when-not read-only
                  #(drag/drag-start! control :level @level level-idx %))}
        (dom/div
          {:class "passthrough" ;; pass drag events through (?)
           ;; :component js/React.DOM.div
           ;; :transitionName "defTrans"
           }
          (let [definition-elements
                (vec (map-indexed
                      (fn [definition-idx definition]
                        (om/build definition/definition-view definition
                                  {:state {:read-only read-only
                                           :path [level-idx definition-idx]
                                           :author-path author-path
                                           :terms terms}}))
                      level))]
            (if drag-target-definition-idx
              (insert definition-elements drag-target-definition-idx
                      (placeholder-definition-element))
              definition-elements)))

        (when-not read-only
          (dom/div
            (dom/button {:class "addDefinition"
                         :on-click #(add-definition! level)} "+def")
            (dom/button {:class "deleteLevel"
                         :on-click #(put! control [:delete-level level-idx])} "x")))))))
