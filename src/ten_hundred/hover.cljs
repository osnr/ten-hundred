(ns ten-hundred.hover
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]))

(defn handle-mouse-enter! [e owner]
  (let [target (.-target e)
        bounding-rect (.getBoundingClientRect target)]
    (om/set-state! owner :hover
                   {:style {:position "fixed"
                            :top (+ (.-top bounding-rect) (.-height bounding-rect))
                            :left (.-left bounding-rect)}})))

(defcomponent span-with-hover-view [data owner]
  (init-state [this]
    {:hover nil})

  (render-state [this {:keys [hover]}]
    (dom/span {:on-mouse-enter #(handle-mouse-enter! % owner)
               :on-mouse-leave #(om/set-state! owner :hover nil)}
      (:content data)
      (when hover
        (dom/div {:class "hover"
                  :style (merge (:hover-style data)
                                (:style hover))}
          (:hover-content data))))))
