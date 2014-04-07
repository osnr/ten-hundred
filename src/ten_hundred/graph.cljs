(ns ten-hundred.graph
  (:require [clojure.string :as string]))

(defn deps [terms level meaning]
  (->> meaning
       (re-seq #"[^\w']+|[\w']+")
       (map (fn [token]
              (let [lc-token (string/lower-case token)]
                (first (filter #(and (< (:level %) level)
                                     (= (:term %) lc-token))
                               terms)))))
       (distinct)
       (remove nil?)))

(defn adjacency-list [terms levels]
  (apply concat (map-indexed
   (fn [idx level]
     (map (fn [{:keys [uuid term meaning focus]}]
            {:uuid uuid

             :term term
             :meaning meaning

             :deps (deps terms idx meaning)
             :focus focus
             :level idx})
          level))
   levels)))

(defn graph [adj-list]
  (let [g (new js/dagre.Digraph)]
    (doseq [{:keys [term focus meaning uuid]} adj-list]
      (let [label (if (empty? term)
                    (str (subs meaning 0 10) "...")
                    term)]
        (.addNode g
                  uuid
                  #js {:label label
                       :focus focus
                       :width (+ 20 (* 8 (count label)))
                       :height 30})))
    (doseq [node adj-list
            dep (:deps node)]
      (.addEdge g
                nil
                (:uuid dep)
                (:uuid node)))
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
