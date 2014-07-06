(ns ten-hundred.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [cljs.core.match]
            [clojure.string :as string]
            [ten-hundred.dict :as dict]
            [ten-hundred.graph :as graph]
            [ten-hundred.terms :as terms]
            [ten-hundred.expansion :as expansion]
            [ten-hundred.docs :as docs]))

(enable-console-print!)

; graph view
(defn render-node [g n value control]
  (let [node (.node g n)
        x (.-x value)
        y (.-y value)
        width (.-width value)
        height (.-height value)

        path (.-path node)
        label (.-label node)]
    (dom/g #js {:transform (str "translate(" (- x (/ width 2))
                                "," (- y (/ height 2)) ")")}
           (dom/rect #js {:className "nodeRect"
                          :onClick #(do (put! control [:focus path])
                                        false)
                          :width width
                          :height height})
           (dom/text #js {:className "nodeLabel"
                          :textAnchor "middle"
                          :x (/ width 2)
                          :y (/ height 2)}
                     label))))

(defn render-edge [layout e]
  (dom/path #js {:className "dep"
                 :d (graph/points->svg-path
                     (graph/calc-points layout e))}))

(defn render-graph [fullscreen-graph terms levels control]
  (let [g (->> levels
               (graph/adjacency-list terms)
               (graph/graph))
        layout (graph/layout g)
        value (.-_value layout)

        width (.-width value)
        height (.-height value)]
    (dom/svg (if fullscreen-graph
               #js {:width width
                    :height height}
               #js {:width 380
                    :height 280
                    :viewBox (str "0 0 "
                                  width " "
                                  height)})
      (js/React.DOM.defs ; <defs> not supported in Om
       #js {:dangerouslySetInnerHTML #js {:__html ; <marker> not supported in React!
            "<marker id=\"markerArrow\" markerWidth=\"6\" markerHeight=\"4\"
                          refx=\"5\" refy=\"2\" orient=\"auto\">
            <path d=\"M 0,0 V 4 L6,2 Z\" class=\"arrow\" />
            </marker>"}})
      (apply dom/g #js {:className "nodes"}
        (map #(render-node g % (.node layout %) control)
             (.nodes layout)))
      (apply dom/g #js {:className "edges"}
        (map #(render-edge layout %)
             (.edges layout))))))

(defn empty-definition []
  {:term ""
   :meaning ""})

(defn make-example-state []
  [[(assoc (empty-definition)
      :term "score"
      :meaning "five times four")
    (assoc (empty-definition)
      :term "continent"
      :meaning "big piece of land")
    (assoc (empty-definition)
      :term "nation"
      :meaning "a people")
    (assoc (empty-definition)
      :term "conceived"
      :meaning "made up")
    (assoc (empty-definition)
      :term "proposition"
      :meaning "an idea that could be right or wrong")]
   [(assoc (empty-definition)
      :term "Gettysburg"
      :meaning (str "Four score and seven years ago our fathers brought forth "
                    "on this continent, a new nation, conceived in Liberty, "
                    "and dedicated to the proposition that all men are created equal."))]
   [(assoc (empty-definition)
      :term "cool"
      :meaning "GettysBurg")]])

; definition view
(defn handle-term-change [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-meaning-change [e owner definition]
  (let [textarea (.-target e)
        bg (om/get-node owner "meaningBg")]
    (js/window.setTimeout 
     (fn [_]
       (let [height (str (.-scrollHeight textarea) "px")]
         (set! (.-height (.-style textarea)) height)
         (set! (.-height (.-style bg)) height)))
     0)
    (om/update! definition :meaning (.-value textarea))))

(defn definition-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [path
                                terms
                                control
                                delete-definition]}]
      (dom/div #js {:className "definition"
                    :id (str "definition_" (string/join "_" path))

                    :draggable true
                    :onDragStart (fn []
                                   (put! control [:drag-start :definition @definition])
                                   (put! delete-definition path))
                    :onDragOver #(put! control [:drag-over :definition path])}
        (dom/div #js {:className "definitionContent"}
          (dom/button #js {:className "expand"
                           :onClick #(put! control [:inspect path])}
                      "i")
          (dom/input #js {:type "text" :placeholder "Term"
                          :className "term"
                          :value (:term definition)
                          :onChange #(handle-term-change % definition)})
          (dom/button #js {:className "remove"
                           :onClick #(put! delete-definition path)}
                      "x")
          (dom/div #js {:className "meaning"}
            (dom/textarea #js {:className "edit"
                               :ref "meaningEdit"
                               :value (:meaning definition)
                               :onChange #(handle-meaning-change % owner definition)})
            (apply dom/pre #js {:className "bg"
                                :ref "meaningBg"}
              (terms/word-map #(terms/colorize-word terms control %)
                              (:meaning definition)))))))))

; level view
(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

(defn splice [v idx]
  (apply conj
         (subvec v 0 idx)
         (subvec v (inc idx))))

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup")))
(defn level-view [level owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete-definition (chan)})

    om/IWillMount
    (will-mount [_]
      (let [delete-definition (om/get-state owner :delete-definition)
            delete-level (om/get-state owner :delete-level)]
        (go-loop []
          (let [[_ idx] (<! delete-definition)]
            (om/transact! level #(splice % idx))
            (recur)))))
    om/IRenderState
    (render-state [this {:keys [level-idx
                                control delete-level terms
                                delete-definition]}]
      (dom/div #js {:className "level"}
        (apply dom/div nil ;; #js {:component js/React.DOM.div
                           ;;      :transitionName "defTrans"}
          (map-indexed (fn [definition-idx definition]
                         (om/build definition-view definition
                                   {:init-state {:control control
                                                 :delete-definition delete-definition}
                                    :state {:path [level-idx definition-idx]
                                            :terms terms}}))
                       level))
        (dom/button #js {:className "addDefinition"
                         :onClick #(add-definition level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! delete-level @level)} "x")))))

; whole-app view and graph pane
(defn add-level! [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [levels delete-level control level-idx level]
  (om/build level-view level
            {:init-state {:control control
                          :delete-level delete-level}
             :state {:level-idx level-idx
                     :terms (terms/find-terms (take level-idx levels))}}))

(defn handle-fullscreen-graph [owner fullscreen-graph]
  (om/update-state! owner :fullscreen-graph not))

(defn log-id [x] (js/console.log (pr-str x)) x)

(defn drop-on [levels definition position uuid]
  (mapv (fn [level]
          (if (seq (filter #(= (:uuid %) uuid) level))
            (let [splice-idx
                  (first (keep-indexed
                          (fn [idx defi]
                            (if (= (:uuid defi) uuid)
                              idx
                              nil))
                          level))]
              (case position
                :above (apply conj
                              (subvec level 0 splice-idx)
                              definition
                              (subvec level splice-idx))
                :below (apply conj
                              (subvec level 0 (inc splice-idx))
                              definition
                              (subvec level (inc splice-idx)))))
            level))
        levels))

(defn drop-on-level [levels definition level-idx]
  (assoc levels level-idx (conj (levels level-idx) definition)))

(defn app-view [levels owner]
  (reify
    om/IInitState
    (init-state [_]
      {:inspect-path nil
       :minimize-sidebar false
       :fullscreen-graph false

       :dragging nil

       :control (chan)
       :delete-level (chan)})

    om/IWillMount
    (will-mount [_]
      (let [control (om/get-state owner :control)
            delete-level (om/get-state owner :delete-level)]
        (go-loop []
          (match (<! control)
            [:focus path] (-> js/document
                              (.getElementById (str "definition_" (string/join "_" path)))
                              (.getElementsByClassName "edit")
                              (aget 0)
                              (.focus))
            [:inspect path] (om/set-state! owner :inspect-path path)

            [:drag-start :definition definition]
            (om/set-state! owner :dragging {:data definition})

            [:drag-over :definition :definition target-uuid position]
            (js/console.log "drag ovah")

            [:drag-over :definition :level target-idx position]
            (js/console.log "drag ovah")

            [:drag-end :definition :definition target-uuid position]
            (om/transact! levels #(drop-on % (om/get-state owner :dragging) position target-uuid))

            [:drag-end :definition :level target-idx]
            (om/transact! levels #(drop-on-level % (om/get-state owner :dragging)) target-idx))
          (recur))

        (go-loop []
          (let [level (<! delete-level)]
            (om/transact! levels
                          (fn [levels] (vec (remove #(= level %) levels))))
            (recur)))))

    om/IRenderState
    (render-state [this {:keys [inspect-path
                                dragging
                                control
                                minimize-sidebar
                                fullscreen-graph
                                delete-level]}]
      (dom/div #js {:className "app"}
        (dom/div #js {:className "levelsWrapper"}
          (apply dom/div #js {:className (str "levels"
                                              (when minimize-sidebar
                                                " minimizeSidebar"))}
                 (conj (vec (map-indexed #(build-level levels delete-level control
                                                       %1 %2)
                                         levels))
                       (dom/button #js {:className "addLevel"
                                        :onClick #(add-level! levels)}
                                   "+level"))))

        (dom/div #js {:className (str "sidebar"
                                      (when minimize-sidebar
                                        " minimize"))}
          (when inspect-path
            (let [inspect-level-idx (first inspect-path)
                  definition (get-in levels inspect-path)]
              (om/build expansion/definition-expansion-view
                        definition
                        {:react-key (str (:uuid definition) "_expansion")
                         :init-state {:expand-to inspect-level-idx
                                      :close #(om/set-state! owner :inspect-path nil)}
                         :state {:level-idx inspect-level-idx
                                 :terms (terms/find-terms (take inspect-level-idx levels))}})))

          (dom/div #js {:className (str "graph"
                                        (when fullscreen-graph
                                          " fullscreen"))
                        :onClick #(handle-fullscreen-graph owner fullscreen-graph)}
            (render-graph fullscreen-graph
                          (terms/find-terms levels)
                          levels
                          control)))

        (dom/div #js {:className "controls"}
          (dom/button #js {:onClick 
                           (fn []
                             (docs/save! @levels)
                             false)}
                      "save")
          (dom/button #js {:onClick
                           (fn []
                             (js/window.open "https://github.com/osnr/ten-hundred")
                             false)}
                      "about")
          (dom/button #js {:onClick #(om/update-state! owner :minimize-sidebar not)}
                      (if minimize-sidebar
                        "^"
                        "v")))))))

(defn init-root [app-state]
  (js/window.history.replaceState "" ""
                                  (str "#/" (:id app-state)))
  (js/console.log (pr-str app-state))
  (om/root app-view
           (atom app-state)
           {:target (. js/document (getElementById "container"))}))

(defn init []
  (let [id (second (string/split (.-URL js/document) #"#/"))]
    (if (empty? id)
      (init-root (make-example-state))
      (docs/load id
                 (fn [loaded-state]
                   (js/console.log (pr-str loaded-state))
                   (init-root loaded-state))
                 (fn []
                   (init-root (assoc (make-example-state)
                                :id id)))))))

(init)
