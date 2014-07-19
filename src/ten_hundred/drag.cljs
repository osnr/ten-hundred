(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.dom.classlist :as classlist]))

(defn definition-drag-move! [control e]
  (.preventDefault e)
  (put! control [:drag-move (.-clientX e) (.-clientY e)])
  (when-let [target (dom/getAncestorByClass (.-target e) "definition")]
    (let [mouse-y (.-clientY e)

          bounding-rect (.getBoundingClientRect target)
          from-top (js/Math.abs (- (.-top bounding-rect) mouse-y))
          from-bottom (js/Math.abs (- (.-bottom bounding-rect) mouse-y))

          [target-level-idx target-definition-idx] (mapv js/parseFloat (rest (string/split (.-id target) #"_")))

          target-path (if (< from-top from-bottom)
                        [target-level-idx target-definition-idx]
                        [target-level-idx (inc target-definition-idx)])]
      (put! control [:drag-over :definition target-path]))))

(defn definition-drag-end! [control]
  (set! (.-onmousemove js/document) nil)
  (set! (.-onmouseup js/document) nil)
  (put! control [:drag-end]))

(defn definition-drag-start! [control definition source-path e]
  (set! (.-onmousemove js/document) #(definition-drag-move! control %))
  (set! (.-onmouseup js/document) #(definition-drag-end! control))
  (let [source (.-target e)
        viewport (dom/getElementByClass "levels")

        mouse-x (.-clientX e)
        mouse-y (.-clientY e)]
    (put! control [:drag-start :definition definition source-path
                   [(- (+ (.-scrollLeft viewport) mouse-x) (.-offsetLeft source))
                    (- (+ (.-scrollTop viewport) mouse-y) (.-offsetTop source))]
                   [mouse-x mouse-y]])))

(def drag-state
  (atom {:dragging nil
         :placeholder
         (let [placeholder (.createElement js/document "div")]
           (set! (.-className placeholder) "placeholder")
           placeholder)}))

(defn drag-start [e target-class target-parent-class data delete-original! control]
  (when (classlist/contains (.-target e) (str target-class "Content"))
    (let [current-target (.-currentTarget e)
          clone (.cloneNode current-target true)

          placeholder (:placeholder @drag-state)

          initial-x (.-offsetLeft current-target)
          initial-y (.-offsetTop current-target)

          listeners {:mousemove #(drag-move % target-class target-parent-class)
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
      (delete-original!))))

(defn drag-move [e target-class target-parent-class]
  (.preventDefault e)
  (let [{:keys [clone offset-x offset-y]} (:dragging @drag-state)

        current-x (.-clientX e)
        current-y (.-clientY e)

        placeholder (:placeholder @drag-state)

        target (.-target e)]
    (set! (.-left (.-style clone))
          (str (- current-x offset-x) "px"))
    (set! (.-top (.-style clone))
          (str (- current-y offset-y) "px"))

    (if-let [dest-target (dom/getAncestorByClass target target-class)]
      (let [parent (.-parentNode dest-target)

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
                           :dest-id (.-id dest-target)})))

      (when-let [parent (and (not (classlist/contains target "clone"))
                             (not (classlist/contains target "placeholder"))
                             (dom/getAncestorByClass target target-parent-class))]
        (.appendChild (.-firstChild parent) placeholder)
        (swap! drag-state
               #(assoc-in % [:dragging :drop-place]
                          {:dest-idx (js/parseInt (last (string/split (.-id parent) #"_")))}))))))

(defn drag-end [e]
  (let [{:keys [clone data drop-place listeners control]} (:dragging @drag-state)
        placeholder (:placeholder @drag-state)]
    (.removeEventListener js/document "mousemove" (:mousemove listeners))
    (.removeEventListener js/document "mouseup" (:mouseup listeners))

    (put! control [:drop-on data drop-place])

    (js/window.setTimeout #(.removeChild (.-parentNode placeholder) placeholder) 0)
    (.removeChild (.-parentNode clone) clone)

    (swap! drag-state #(dissoc % :dragging))))
