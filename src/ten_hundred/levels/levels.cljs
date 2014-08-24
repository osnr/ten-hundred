(ns ten-hundred.levels.levels
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [ten-hundred.util :refer [insert]]
            [ten-hundred.terms :as terms]
            [ten-hundred.levels.level :as level]
            [ten-hundred.levels.definition :as definition]))

(defn placeholder-level-element []
  (dom/div {:class "placeholderLevel"}))

(defn add-level! [app]
  (om/transact! app #(conj % [(definition/empty-definition)])))

(defn build-level [levels control read-only
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

(defcomponent levels-view [levels owner]
  (render-state [this {:keys [dragging author-path]}]
    (let [control (om/get-shared owner :control)]
      (dom/div nil
        (conj (let [level-elements
                    (vec (map-indexed
                          #(build-level levels
                                        control
                                        read-only
                                        author-path
                                        (when (= (:data-kind dragging) :definition)
                                          (:target-path dragging))
                                        %1 %2)
                          levels))]
                (if (= (:data-kind dragging) :level)
                  (insert level-elements (:target-path dragging) (placeholder-level-element))
                  level-elements))
              (when-not read-only
                (dom/button {:class "addLevel"
                             :on-click #(add-level! levels)}
                            "+level")))))))
