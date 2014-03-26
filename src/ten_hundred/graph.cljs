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

(defn points->svg-path [points]
  (let [start (first points)
        control (second points)
        end (last points)]
    (->> points
         (rest)
         (butlast)
         (rest)
         (mapv (fn [p] (str "T " (.-x p) " " (.-y p))))
         (cons (str "Q " (.-x control) " " (.-y control)
                    " " (.-x end) " " (.-y end)))
         (cons (str "M " (.-x start) " " (.-y start)))
         (string/join " "))))


