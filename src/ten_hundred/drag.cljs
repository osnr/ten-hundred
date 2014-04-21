(ns ten-hundred.drag
  (:require [om.core :as om :include-macros true]
            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.style :as style]
            [goog.dom.classlist :as classlist]))

(def drag-state
  (atom {:dragging nil
         :placeholder
         (let [placeholder (.createElement js/document "div")]
           (set! (.-className placeholder) "placeholder")
           placeholder)}))

(defn drag-start [e target-class drag-axis target-parent-class
                  data delete-original! control]
  (when (classlist/contains (.-target e) (str target-class "Content"))
    (let [original-target (.-currentTarget e)
          clone (.cloneNode original-target true)

          placeholder (:placeholder @drag-state)

          initial-x (.-offsetLeft original-target)
          initial-y (.-offsetTop original-target)

          listeners {:mousemove #(drag-move % target-class drag-axis target-parent-class)
                     :mouseup drag-end}]
      (swap! drag-state
             #(assoc %
                :dragging
                {:clone clone
                 :data data

                 :target-class target-class

                 :offset-x (- (.-clientX e) initial-x)
                 :offset-y (- (.-clientY e) initial-y)

                 :drop-place {:position nil
                              :dest-idx nil}

                 :listeners listeners
                 :control control}))
      (set! (.-width (.-style clone))
            (str (.-width original-target) "px"))
      (set! (.-height (.-style clone))
            (str (.-height original-target) "px"))
      (set! (.-left (.-style clone))
            (str initial-x "px"))
      (set! (.-top (.-style clone))
            (str initial-y "px"))
      (set! (.-className clone)
            (str (.-className original-target) " clone"))
      (.appendChild (dom/getElementByClass "levels") clone)

      (set! (.-width (.-style placeholder))
            (str (.-clientWidth original-target) "px"))
      (set! (.-height (.-style placeholder))
            (str (.-clientHeight original-target) "px"))
      (set! (.-display (.-style placeholder))
            (style/getComputedStyle original-target "display"))

      (.addEventListener js/document "mousemove" (:mousemove listeners))
      (.addEventListener js/document "mouseup" (:mouseup listeners))
      (delete-original!)

      (js/window.setTimeout #(.insertBefore (.-parentNode original-target)
                                            placeholder
                                            (.-nextSibling original-target))
                            0))))

(defn- dom-id->dest-id [el]
  (let [id (.-id el)]
    (if (re-matches #"[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}" id)
      id
      (js/parseInt (last (string/split (.-id el) #"_"))))))

(defn drag-move [e target-class drag-axis target-parent-class]
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
            from-start (js/Math.abs (case drag-axis
                                      :x (- (.-left bounding-rect) current-x)
                                      :y (- (.-top bounding-rect) current-y)))
            from-end (js/Math.abs (case drag-axis
                                    :x (- (.-right bounding-rect) current-x)
                                    :y (- (.-bottom bounding-rect) current-y)))

            position (if (< from-start from-end)
                       :before
                       :after)]
        (case position
          :before (.insertBefore parent placeholder dest-target)
          :after (.insertBefore parent placeholder (.-nextSibling dest-target)))

        (swap! drag-state
               #(assoc-in % [:dragging :drop-place]
                          {:position position
                           :dest-id (dom-id->dest-id dest-target)})))

      (when-let [parent (and (not (classlist/contains target "clone"))
                             (not (classlist/contains target "placeholder"))
                             (dom/getAncestorByClass target target-parent-class))]
        (.appendChild (.-firstChild parent) placeholder)
        (swap! drag-state
               #(assoc-in % [:dragging :drop-place]
                          {:dest-idx (dom-id->dest-id parent)}))))))

(defn drag-end [e]
  (let [{:keys [clone data target-class drop-place listeners control]} (:dragging @drag-state)
        placeholder (:placeholder @drag-state)]
    (.removeEventListener js/document "mousemove" (:mousemove listeners))
    (.removeEventListener js/document "mouseup" (:mouseup listeners))

    (put! control [:drop-on target-class data drop-place])

    (js/window.setTimeout #(.removeChild (.-parentNode placeholder) placeholder) 0)
    (.removeChild (.-parentNode clone) clone)

    (swap! drag-state #(dissoc % :dragging))))
