(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]))

(def drag-state
  (atom {:dragging nil
         :drop! nil
         :placeholder
         (let [placeholder (.createElement js/document "div")]
           (set! (.-className placeholder) "placeholder")
           placeholder)}))

(defn drag-start [e definition delete-original!]
  (js/console.log "drag-start")
  (let [current-target (.-currentTarget e)
        data-transfer (.-dataTransfer e)]
    (swap! drag-state #(assoc % :dragging definition))
    (set! (.-effectAllowed data-transfer) "move")
    (.setData data-transfer "text/html" current-target)
    (delete-original!)))

(defn drag-end [e]
  (let [dragging (:dragging @drag-state)
        drop! (:drop! @drag-state)
        placeholder (:placeholder @drag-state)]
    (.removeChild (.-parentNode placeholder) placeholder)
    (drop!)
    (swap! drag-state #(dissoc % :dragging :drop!))))

(defn drag-over [e owner drop]
  (let [target (om/get-node owner)
        parent (.-parentNode target)

        dragging (:dragging @drag-state)
        placeholder (:placeholder @drag-state)

        bounding-rect (.getBoundingClientRect target)
        from-top (js/Math.abs (- (.-top bounding-rect) (.-pageY e)))
        from-bottom (js/Math.abs (- (.-bottom bounding-rect) (.-pageY e)))

        position (if (< from-top from-bottom)
                   :above
                   :below)]
    (case position
      :above (.insertBefore parent placeholder target)
      :below (.insertBefore parent placeholder (.-nextSibling target)))

    (swap! drag-state #(assoc % :drop! (fn [] (drop dragging position))))))

