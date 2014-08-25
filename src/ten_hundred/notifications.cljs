(ns ten-hundred.notifications
  (:require-macros [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.match]
            [ten-hundred.util :refer [splice]]))

(defn render-notification [notification]
  (match notification
    [:published publish-url]
    (dom/span
      "Published to "
      (dom/a {:href publish-url} publish-url))))

(defcomponent notifications-view [notifications owner]
  (render [_]
    (dom/div {:class "notifications"}
      (map-indexed (fn [idx notification]
                     (dom/div {:class "notification"
                               :on-click (fn [e] (om/transact! notifications #(splice % idx)))}
                       (render-notification notification)))
                   notifications))))
