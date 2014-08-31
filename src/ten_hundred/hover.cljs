(ns ten-hundred.hover
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [goog.dom]))

(defn handle-mouse-move! [e owner]
  (when-let [bounds (om/get-state owner [:hover :bounds])]
    (when-not (and (>= (.-clientX e) (- (.-left bounds) 1))
                   (<= (.-clientX e) (+ (.-right bounds) 1))
                   (>= (.-clientY e) (- (.-top bounds) 1))
                   (<= (.-clientY e) (+ (.-bottom bounds) 1)))
      (om/set-state! owner :hover nil)

      (js/document.removeEventListener "mousemove" (.-mouse-move-listener owner))
      (set! (.-mouse-move-listener owner) nil))))

(defn handle-mouse-enter! [e owner]
  (let [target (goog.dom/getAncestorByClass (.-target e) "hoverView")
        bounds (.getBoundingClientRect target)]
    (om/set-state! owner :hover
                   {:bounds bounds})
    (set! (.-mouse-move-listener owner) #(handle-mouse-move! % owner))
    (js/document.addEventListener "mousemove" (.-mouse-move-listener owner))))

(defcomponent span-with-hover-view [data owner]
  (init-state [this]
    {:hover nil})

  (render-state [this {:keys [hover]}]
    (dom/span {:class (str "hoverView" (when hover " hovering"))
               :on-mouse-enter #(handle-mouse-enter! % owner)}
      (:content data)
      (when-let [bounds (:bounds hover)]
        (dom/div {:class "hover"
                  :style (merge (:hover-style data)
                                {:position "fixed"
                                 :top (+ (.-top bounds) (.-height bounds))
                                 :left (.-left bounds)})}
          (let [hover-content (:hover-content data)]
            (if (.-call hover-content)
              (hover-content)
              hover-content)))))))
