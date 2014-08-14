(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.dom.classlist :as classlist]))

(defn drag-clone-style [drag-clone]
  (if drag-clone
    (let [[x y] (:pos drag-clone)
          transform (str "translate(" x "px," y "px)")]
      {:pointer-events "none"
       :position "absolute"
       :left 0
       :top 0
       :-webkit-transform transform
       :-moz-transform transform
       :-ms-transform transform
       :transform transform })
    {}))

(defn definition-drag-move! [control e]
  (if-let [target (dom/getAncestorByClass (.-target e) "definition")]
    (let [mouse-y (.-clientY e)

          bounding-rect (.getBoundingClientRect target)
          from-top (js/Math.abs (- (.-top bounding-rect) mouse-y))
          from-bottom (js/Math.abs (- (.-bottom bounding-rect) mouse-y))

          [target-level-idx target-definition-idx]
          (mapv js/parseFloat (rest (string/split (.-id target) #"_")))

          final-target-path (if (< from-top from-bottom)
                              [target-level-idx target-definition-idx]
                              [target-level-idx (inc target-definition-idx)])]
      (put! control [:drag-over final-target-path]))
    (when (classlist/contains (.-target e) "level")
      (let [target (.-target e)]
        (put! control [:drag-over [(-> (.-id target)
                                       (string/split #"_")
                                       second
                                       js/parseFloat)
                                   0]])))))

(defn level-drag-move! [control e]
  (when-let [target (dom/getAncestorByClass (.-target e) "level")]
    (let [mouse-x (.-clientX e)

          bounding-rect (.getBoundingClientRect target)
          from-left (js/Math.abs (- (.-left bounding-rect) mouse-x))
          from-right (js/Math.abs (- (.-right bounding-rect) mouse-x))

          target-level-idx (-> (.-id target)
                               (string/split #"_")
                               second
                               js/parseFloat)

          final-target-level-idx (if (< from-left from-right)
                                   target-level-idx
                                   (inc target-level-idx))]
      (put! control [:drag-over final-target-level-idx]))))

(def scroll-timer (atom nil))
(defn drag-move! [control data-kind e]
  (.preventDefault e)
  (js/clearInterval @scroll-timer)
  (let [mouse-x (.-clientX e)
        mouse-y (.-clientY e)

        viewport (dom/getElementByClass "levels")
        viewport-bottom (.-offsetHeight viewport)
        viewport-right (.-offsetWidth viewport)

        scroll-y (cond (< (- viewport-bottom mouse-y) 80) 20
                       (< mouse-y 80) -20)
        scroll-x (cond (< (- viewport-right mouse-x) 80) 20
                       (< mouse-x 80) -20)]
    (swap! scroll-timer
           (constantly
            (js/setInterval
             (fn []
               (set! (.-scrollTop viewport)
                     (+ scroll-y (.-scrollTop viewport)))
               (set! (.-scrollLeft viewport)
                     (+ scroll-x (.-scrollLeft viewport))))
             50)))
    (put! control [:drag-move mouse-x mouse-y]))

  (case data-kind
    :definition (definition-drag-move! control e)
    :level (level-drag-move! control e)))

(defn drag-end! [control]
  (js/clearInterval @scroll-timer)
  (set! (.-onmousemove js/document) nil)
  (set! (.-onmouseup js/document) nil)
  (put! control [:drag-end]))

; source-path is [level-idx definition-idx] if dragging a definition
;                level-idx if dragging a level
(defn drag-start! [control data-kind data source-path e]
  (let [of-kind? #(classlist/contains % (name data-kind))]
    (when-let [source (cond (of-kind? (.-target e)) (.-target e)

                            (and (classlist/contains (.-target e) "passthrough")
                                 (of-kind? (.-parentElement (.-target e))))
                            (.-parentElement (.-target e)))]
      (set! (.-onmousemove js/document) #(drag-move! control data-kind %))
      (set! (.-onmouseup js/document) #(drag-end! control))
      (let [viewport (dom/getElementByClass "levels")
            viewport-bounds (.getBoundingClientRect viewport)
            viewport-left (.-left viewport-bounds)
            viewport-top (.-top viewport-bounds)

            mouse-x (.-clientX e)
            mouse-y (.-clientY e)]
        (put! control [:drag-start data-kind data source-path
                       [(- (+ (.-scrollLeft viewport) mouse-x)
                           (+ viewport-left (.-offsetLeft source)))
                        (- (+ (.-scrollTop viewport) mouse-y)
                           (+ viewport-top  (.-offsetTop source)))]
                       [mouse-x mouse-y]])))))
