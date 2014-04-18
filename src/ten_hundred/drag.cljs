(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]))

(def drag-state
  (atom {:dragging {:clone nil
                    :definition nil
                    :offset-x 0
                    :offset-y 0
                    :drop-on nil}
         :placeholder
         (let [placeholder (.createElement js/document "div")]
           (set! (.-className placeholder) "placeholder")
           placeholder)}))

(defn drag-start [e definition delete-original! drop-on]
  (js/console.log "drag-start")
  (let [current-target (.-currentTarget e)
        clone (.cloneNode current-target true)
        data-transfer (.-dataTransfer e)

        initial-x (.-offsetLeft current-target)
        initial-y (.-offsetTop current-target)]
    (swap! drag-state
           #(assoc %
              :dragging
              {:clone clone
               :definition definition
               :offset-x (- (.-clientX e) initial-x)
               :offset-y (- (.-clientY e) initial-y)
               :drop-on drop-on}))
    (set! (.-width (.-style clone))
          (str (.-width current-target) "px"))
    (set! (.-height (.-style clone))
          (str (.-height current-target) "px"))
    (set! (.-left (.-style clone))
          (str initial-x "px"))
    (set! (.-top (.-style clone))
          (str initial-y "px"))
    (set! (.-className clone)
          (str (.-className current-target) " draggingEl"))
    (.appendChild js/document.body clone)

    (.addEventListener js/document "mousemove" drag-move)
    (.addEventListener js/document "mouseup" drag-end)
    (delete-original!)))

(defn drag-end [e]
  (let [dragging (:dragging @drag-state)
        placeholder (:placeholder @drag-state)

        clone (:clone dragging)
        drop! (:drop! dragging)]
    (.removeEventListener js/document "mousemove" drag-move)
    (.removeEventListener js/document "mouseup" drag-end)

    (.removeChild (.-parentNode placeholder) placeholder)
    (.removeChild (.-parentNode clone) clone)
    (drop!)
    (swap! drag-state #(dissoc % :dragging))))

(defn drag-move [e owner drop]
  (.preventDefault e)
  (let [dragging (:dragging @drag-state)

        clone (:clone dragging)

        offset-x (:offset-x dragging)
        offset-y (:offset-y dragging)

        current-x (.-clientX e)
        current-y (.-clientY e)]
    (set! (.-left (.-style clone))
          (str (- current-x offset-x) "px"))
    (set! (.-top (.-style clone))
          (str (- current-y offset-y) "px"))

    (when-let [definition-target
               (loop [elem (.-target e)]
                 (when elem
                   (if (> (.indexOf (str " " (.-className elem) " ")
                                    " definition ")
                          -1)
                     elem
                     (recur (.-parentNode elem)))))]
      (let [parent (.-parentNode definition-target)

            placeholder (:placeholder @drag-state)

            bounding-rect (.getBoundingClientRect definition-target)
            from-top (js/Math.abs (- (.-top bounding-rect) (.-pageY e)))
            from-bottom (js/Math.abs (- (.-bottom bounding-rect) (.-pageY e)))

            position (if (< from-top from-bottom)
                       :above
                       :below)]
        (case position
          :above (.insertBefore parent placeholder definition-target)
          :below (.insertBefore parent placeholder (.-nextSibling definition-target)))

        (swap! drag-state #(assoc-in % [:dragging :drop!]
                                     (fn [] (drop dragging position))))))))

