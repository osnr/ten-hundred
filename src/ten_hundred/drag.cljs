(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.dom.classlist :as classlist]))

(defn definition-drag-move! [control e]
  (.preventDefault e)
  (put! control [:drag-move (.-clientX e) (.-clientY e)])
  (if-let [target (dom/getAncestorByClass (.-target e) "definition")]
    (let [mouse-y (.-clientY e)

          bounding-rect (.getBoundingClientRect target)
          from-top (js/Math.abs (- (.-top bounding-rect) mouse-y))
          from-bottom (js/Math.abs (- (.-bottom bounding-rect) mouse-y))

          [target-level-idx target-definition-idx] (mapv js/parseFloat (rest (string/split (.-id target) #"_")))

          target-path (if (< from-top from-bottom)
                        [target-level-idx target-definition-idx]
                        [target-level-idx (inc target-definition-idx)])]
      (put! control [:drag-over :definition target-path]))
    (when (classlist/contains (.-target e) "level")
      (let [target (.-target e)]
        (put! control [:drag-over :definition [(-> (.-id target)
                                                   (string/split #"_")
                                                   second
                                                   js/parseFloat)
                                               0]])))))

(defn definition-drag-end! [control]
  (set! (.-onmousemove js/document) nil)
  (set! (.-onmouseup js/document) nil)
  (put! control [:drag-end]))

(defn definition-drag-start! [control definition source-path e]
  (when (classlist/contains (.-target e) "definitionContent")
    (set! (.-onmousemove js/document) #(definition-drag-move! control %))
    (set! (.-onmouseup js/document) #(definition-drag-end! control))
    (let [source (.-target e)
          viewport (dom/getElementByClass "levels")

          mouse-x (.-clientX e)
          mouse-y (.-clientY e)]
      (put! control [:drag-start :definition definition source-path
                     [(- (+ (.-scrollLeft viewport) mouse-x) (.-offsetLeft source))
                      (- (+ (.-scrollTop viewport) mouse-y) (.-offsetTop source))]
                     [mouse-x mouse-y]]))))
