(ns ten-hundred.levels.levels
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ten-hundred.util :refer [insert]]
            [ten-hundred.terms :as terms]
            [ten-hundred.levels.level :as level]
            [ten-hundred.levels.definition :as definition]))

(defn placeholder-level-element []
  (dom/div #js {:className "placeholderLevel"}))

(defn add-level! [app]
  (om/transact! app #(conj % [(definition/empty-definition)])))

(defn build-level [levels control
                   author-path
                   [drag-target-level-idx drag-target-definition-idx]
                   level-idx level]
  (om/build level/level-view level
            {:init-state {:control control}
             :state {:level-idx level-idx
                     :author-path author-path
                     :drag-target-definition-idx
                     (when (= level-idx drag-target-level-idx)
                       drag-target-definition-idx)
                     :terms (terms/find-terms (take level-idx levels))}}))

(defn levels-view [levels owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [dragging control author-path]}]
      (apply dom/div nil
             (conj (let [level-elements
                         (vec (map-indexed
                               #(build-level levels
                                             control
                                             author-path
                                             (when (= (:data-kind dragging) :definition)
                                               (:target-path dragging))
                                             %1 %2)
                               levels))]
                     (if (= (:data-kind dragging) :level)
                       (insert level-elements (:target-path dragging) (placeholder-level-element))
                       level-elements))
                   (dom/button #js {:className "addLevel"
                                    :onClick #(add-level! levels)}
                               "+level"))))))

