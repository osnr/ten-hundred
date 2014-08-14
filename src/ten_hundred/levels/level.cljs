(ns ten-hundred.levels.level
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put!]]
            [ten-hundred.util :refer [insert]]
            [ten-hundred.drag :as drag]
            [ten-hundred.levels.definition :as definition]))

(defn placeholder-definition-element []
  (dom/div #js {:className "placeholderDefinition"}))

(defn add-definition! [level]
  (om/transact! level #(conj % (definition/empty-definition))))

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup")))
(defn level-view [level owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [level-idx
                                drag-clone author-path
                                drag-target-definition-idx
                                control terms]}]
      (dom/div #js {:className "level"
                    :id (str "level_" level-idx)
                    :style (drag/drag-clone-style drag-clone)

                    :onMouseDown #(drag/drag-start! control :level @level level-idx %)}
        (apply dom/div
          #js {:className "passthrough"
               ;; :component js/React.DOM.div
               ;; :transitionName "defTrans"
               }
          (let [definition-elements
                (vec (map-indexed
                      (fn [definition-idx definition]
                        (om/build definition/definition-view definition
                                  {:init-state {:control control}
                                   :state {:path [level-idx definition-idx]
                                           :author-path author-path
                                           :terms terms}}))
                      level))]
            (if drag-target-definition-idx
              (insert definition-elements drag-target-definition-idx
                      (placeholder-definition-element))
              definition-elements)))
        (dom/button #js {:className "addDefinition"
                         :onClick #(add-definition! level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! control [:delete-level level-idx])} "x")))))
