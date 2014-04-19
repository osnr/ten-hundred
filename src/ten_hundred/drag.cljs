(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :refer [put!]]
            [goog.dom :as dom]
            [goog.dom.classlist :as classlist]))

(def drag-state
  (atom {:dragging nil
         :placeholder
         (let [placeholder (.createElement js/document "div")]
           (set! (.-className placeholder) "placeholder")
           placeholder)}))

(defn drag-start [e target-class data delete-original! control]
  (when (classlist/contains (.-target e) target-class)
    (let [current-target (.-currentTarget e)
          clone (.cloneNode current-target true)

          placeholder (:placeholder @drag-state)

          initial-x (.-offsetLeft current-target)
          initial-y (.-offsetTop current-target)

          listeners {:mousemove #(drag-move % target-class)
                     :mouseup drag-end}]
      (swap! drag-state
             #(assoc %
                :dragging
                {:clone clone
                 :data data

                 :offset-x (- (.-clientX e) initial-x)
                 :offset-y (- (.-clientY e) initial-y)

                 :drop-place {:position nil
                              :dest-id nil}

                 :listeners listeners
                 :control control}))
      (set! (.-width (.-style clone))
            (str (.-width current-target) "px"))
      (set! (.-height (.-style clone))
            (str (.-height current-target) "px"))
      (set! (.-left (.-style clone))
            (str initial-x "px"))
      (set! (.-top (.-style clone))
            (str initial-y "px"))
      (set! (.-className clone)
            (str (.-className current-target) " clone"))
      (.appendChild (dom/getElementByClass "levels") clone)

      (.addEventListener js/document "mousemove" (:mousemove listeners))
      (.addEventListener js/document "mouseup" (:mouseup listeners))
      (delete-original!)

      (.insertBefore (.-parentNode current-target) placeholder (.-nextSibling current-target)))))

(defn drag-move [e target-class]
  (.preventDefault e)
  (let [{:keys [clone offset-x offset-y]} (:dragging @drag-state)

        current-x (.-clientX e)
        current-y (.-clientY e)]
    (set! (.-left (.-style clone))
          (str (- current-x offset-x) "px"))
    (set! (.-top (.-style clone))
          (str (- current-y offset-y) "px"))

    (when-let [dest-target (dom/getAncestorByClass (.-target e) target-class)]
      (let [parent (.-parentNode dest-target)

            placeholder (:placeholder @drag-state)

            bounding-rect (.getBoundingClientRect dest-target)
            from-top (js/Math.abs (- (.-top bounding-rect) current-y))
            from-bottom (js/Math.abs (- (.-bottom bounding-rect) current-y))

            position (if (< from-top from-bottom)
                       :above
                       :below)]
        (case position
          :above (.insertBefore parent placeholder dest-target)
          :below (.insertBefore parent placeholder (.-nextSibling dest-target)))

        (swap! drag-state
               #(assoc-in % [:dragging :drop-place]
                          {:position position
                           :dest-id (.-id dest-target)}))))))

(defn drag-end [e]
  (let [{:keys [clone data drop-place listeners control]} (:dragging @drag-state)
        {:keys [position dest-id]} drop-place
        placeholder (:placeholder @drag-state)]
    (.removeEventListener js/document "mousemove" (:mousemove listeners))
    (.removeEventListener js/document "mouseup" (:mouseup listeners))

    (put! control [:drop-on data position dest-id])

    (js/window.setTimeout #(.removeChild (.-parentNode placeholder) placeholder) 0)
    (.removeChild (.-parentNode clone) clone)

    (swap! drag-state #(dissoc % :dragging))))
