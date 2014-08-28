(ns ten-hundred.notifications
  (:require-macros [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.match]
            [ten-hundred.util :refer [splice]]))

(defn render-notification [notification]
  (case (first notification)
    :saved
    (dom/div
      (dom/p
       "Saved to new link "
       (dom/strong (dom/a {:href (second notification)} (second notification)))
       ".")
      (dom/p
       "You can bookmark that link to edit this document later."))

    :published
    (dom/div
      (dom/p
       "Published to "
       (dom/strong (dom/a {:href (second notification)} (second notification)))
       ".")
      (dom/p
       "If you share that link with people, they can view, not change, this document. Save "
       (dom/a {:href (get notification 2)} "your original link")
       " to edit this document later:"))))

(defcomponent notifications-view [notifications owner]
  (render [_]
    (dom/div {:class "notifications"}
      (map-indexed (fn [idx notification]
                     (dom/div {:class "notification"
                               :on-click (fn [e] (om/transact! notifications #(splice % idx)))}
                       (render-notification notification)))
                   notifications))))
