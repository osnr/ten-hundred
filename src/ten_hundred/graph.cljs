(ns ten-hundred.graph
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [clojure.string :as string]
            [cljs.core.async :refer [put!]]
            [ten-hundred.terms :as terms]))

(defn deps [terms level-idx meaning]
  (->> meaning
       (js/THParser.parse)
       (filter #(= (first %) "word"))
       (map (fn [[_ word]]
              (let [lc-word (string/lower-case word)]
                (first (filter #(and (< (first (:path %)) level-idx)
                                     (= (:term %) lc-word))
                               terms)))))
       (distinct)
       (remove nil?)))

(defn adjacency-list [terms levels]
  (apply concat
   (map-indexed
    (fn [level-idx level]
      (map-indexed
        (fn [definition-idx {:keys [term meaning]}]
          {:path [level-idx definition-idx]

           :term term
           :meaning meaning

           :deps (deps terms level-idx meaning)})
        level))
    levels)))

(defn graph [adj-list]
  (let [g (new js/dagre.Digraph)]
    (doseq [{:keys [term meaning path]} adj-list]
      (let [label (if (empty? term)
                    (str (subs meaning 0 10) "...")
                    term)]
        (.addNode g
                  (string/join "." path)
                  #js {:label label
                       :path path
                       :width (+ 20 (* 8 (count label)))
                       :height 30})))
    (doseq [node adj-list
            dep (:deps node)]
      (.addEdge g
                nil
                (string/join "." (:path dep))
                (string/join "." (:path node))))
    g))

(defn layout [g]
  (-> js/dagre
      (.layout)
      (.rankDir "LR")
      (.run g)))

(defn intersect-rect [rect point]
  (let [x (.-x rect)
        y (.-y rect)

        dx (- (.-x point) x)
        dy (- (.-y point) y)

        w (/ (.-width rect) 2)
        h (/ (.-height rect) 2)]
    (if (> (* (js/Math.abs dy) w)
           (* (js/Math.abs dx) h))
      ;; intersection is top or bottom of rect
      (let [h (if (< dy 0)
                (- h)
                h)]
        #js {:x (+ x (if (= dy 0)
                       0
                       (* h (/ dx dy))))
             :y (+ y h)})

      ;; intersection is left or right of rect
      (let [w (if (< dx 0)
                (- w)
                w)]
        #js {:x (+ x w)
             :y (+ y (if (= dx 0)
                       0
                       (* w (/ dy dx))))}))))

(defn calc-points [layout e]
  (let [value (.edge layout e)
        source (.node layout (first (.incidentNodes layout e)))
        target (.node layout (second (.incidentNodes layout e)))
        points (vec (.-points value))

        p0 (if (empty? points)
             target
             (first points))
        p1 (if (empty? points)
             source
             (last points))]
    (cons (intersect-rect source p0)
          (conj points
                (intersect-rect target p1)))))

(defn- control-points [[x0 y0] [x1 y1] [x2 y2] t]
  (let [d01 (js/Math.sqrt (+ (js/Math.pow (- x1 x0) 2)
                             (js/Math.pow (- y1 y0) 2)))
        d12 (js/Math.sqrt (+ (js/Math.pow (- x2 x1) 2)
                             (js/Math.pow (- y2 y1) 2)))

        fa (/ (* t d01)
              (+ d01 d12))
        fb (- t fa)

        p1x (+ x1 (* fa (- x0 x2)))
        p1y (+ y1 (* fa (- y0 y2)))

        p2x (- x1 (* fb (- x0 x2)))
        p2y (- y1 (* fb (- y0 y2)))]
    [[p1x p1y]
     [p2x p2y]]))

(defn js-point->vec [p]
  [(.-x p)
   (.-y p)])

(defn points->svg-path [points]
  (let [points (map js-point->vec points)
        [fx fy] (first points)
        [sx sy] (second points)
        [lx ly] (last points)
        [slx sly] (last (butlast points))

        cubic-points
        (->> points
             (partition 3 2)
             (mapv (fn [[p0 p1 p2]]
                     (let [[cp0 cp1]
                           (control-points p0 p1 p2 0.5)]
                       [cp0 cp1 p2]))))

        [cpsx cpsy] (first (first cubic-points))
        [cplx cply] (second (last cubic-points))]
    (str "M " fx " " fy " "
         "Q " cpsx " " cpsy
         " " sx " " sy
         (string/join
          " "
          (mapv (fn [[[cp0x cp0y] [cp1x cp1y] [px py]]]
                  (str "C " cp0x " " cp0y
                       " " cp1x " " cp1y
                       " " px " " py))
                (butlast cubic-points)))
         "Q " cplx " " cply " "
         " " lx " " ly)))

(defn render-node [author-path control
                   g n value]
  (let [node (.node g n)
        x (.-x value)
        y (.-y value)
        width (.-width value)
        height (.-height value)

        path (.-path node)
        label (.-label node)]
    (dom/g {:transform (str "translate(" (- x (/ width 2))
                            "," (- y (/ height 2)) ")")
            :class (if (= author-path path)
                         "authoring node"
                         "node")
            :key path}
      (dom/rect {:class "nodeRect"
                 :on-click #(do (put! control [:author path])
                                false)
                 :width width
                 :height height})
      (dom/text {:class "nodeLabel"
                 :text-anchor "middle"
                 :x (/ width 2)
                 :y (/ height 2)}
                label))))

(defn render-edge [layout e]
  (dom/path {:class "dep"
                 :d (points->svg-path (calc-points layout e))}))

(defcomponent graph-view [levels owner]
  (init-state [_]
    {:minimize-graph false
     :fullscreen-graph false})

  (render-state [this {:keys [minimize-graph fullscreen-graph
                              author-path control]}]
    (dom/div {:class (str "graph"
                              (when fullscreen-graph
                                " fullscreen"))
              :on-click #(om/update-state! owner :fullscreen-graph not)}
      (let [terms (terms/find-terms levels)

            g (->> levels
                   (adjacency-list terms)
                   (graph))
            layout (layout g)
            value (.-_value layout)

            width (.-width value)
            height (.-height value)]
        (dom/svg (cond minimize-graph
                       {:style {:display "none"}}

                       fullscreen-graph 
                       {:width width
                        :height height}

                       :else
                       {:width 380
                        :height 280
                        :view-box (str "0 0 "
                                       (max width 380) " "
                                       (max height 280))})
          (js/React.DOM.defs ; <defs> not supported in Om
           #js {:dangerouslySetInnerHTML #js {:__html ; <marker> not supported in React!
                                              "<marker id=\"markerArrow\" markerWidth=\"6\" markerHeight=\"4\"
                                                       refx=\"5\" refy=\"2\" orient=\"auto\">
                                                   <path d=\"M 0,0 V 4 L6,2 Z\" class=\"arrow\" />
                                               </marker>"}})
          (dom/g {:class "nodes"}
            (map #(render-node author-path control
                               g % (.node layout %))
                 (.nodes layout)))
          (dom/g {:class "edges"}
            (map #(render-edge layout %)
                 (.edges layout)))))

      (dom/div {:class "graphControls"}
        (dom/button {:on-click (fn [_]
                                 (om/update-state! owner :minimize-graph not)
                                 false)}
                    (case minimize-graph
                      true (dom/i {:class "fa fa-angle-up"})
                      false (dom/i {:class "fa fa-angle-down"})))))))
