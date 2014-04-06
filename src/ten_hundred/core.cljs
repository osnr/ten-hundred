(ns ten-hundred.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]
            [cljs-uuid-utils :as uuid]
            [ten-hundred.dict :as dict]
            [ten-hundred.graph :as graph]))

(enable-console-print!)

(defn handle-node-click [focus]
  (put! focus :focus)
  false)

(defn render-node [g n value]
  (let [node (.node g n)
        x (.-x value)
        y (.-y value)
        width (.-width value)
        height (.-height value)

        label (.-label node)
        focus (.-focus node)]
    (dom/g #js {:transform (str "translate(" (- x (/ width 2))
                                "," (- y (/ height 2)) ")")}
           (dom/rect #js {:className "nodeRect"
                          :onClick #(handle-node-click focus)
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

(defn render-graph [graph-state terms levels]
  (when (not= graph-state :hide)
    (let [g (->> levels
                 (graph/adjacency-list terms)
                 (graph/graph))
          layout (graph/layout g)
          value (.-_value layout)

          width (.-width value)
          height (.-height value)]
      (dom/svg (if (= graph-state :fullscreen)
                 #js {:width width
                      :height height}
                 #js {:width width
                      :height 250
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
          (map #(render-node g % (.node layout %))
               (.nodes layout)))
        (apply dom/g #js {:className "edges"}
          (map #(render-edge layout %)
               (.edges layout)))))))

(defn handle-term-click [term-state]
  (put! (:focus term-state) :focus))

(defn find-term [terms token]
  (last (filter #(= (:term %) token) terms)))

(defn colorize-token [terms token]
  (let [lc-token (string/lower-case token)]
    (cond (re-matches #"[^\w']+" token) token

          (and terms (find-term terms lc-token))
          (dom/span #js {:className "defined"
                         :onClick #(handle-term-click
                                    (find-term terms lc-token))}
                    token)

          (dict/words lc-token) token

          :else (dom/span #js {:className "uncommon"} token))))

(defn colorize [terms text]
  (->> text
       (re-seq #"[^\w']+|[\w']+")
       (map #(colorize-token terms %))))

(defn empty-definition []
  {:uuid (uuid/uuid-string (uuid/make-random-uuid))
   :term ""
   :meaning ""
   :focus (chan)})

;; (def app-state
;;   (atom [[(empty-definition)]]))

(def app-state
  (atom [[(assoc (empty-definition)
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
                          "and dedicated to the proposition that all men are created equal."))]]))

(defn handle-term-change [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-meaning-change [e definition]
  (om/update! definition :meaning (.. e -target -value)))

(defn definition-view [definition owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [focus (:focus definition)]
        (go (loop []
          (let [e (<! focus)]
            (cond (= e :focus) (-> (om/get-node owner)
                                   (.getElementsByClassName "edit")
                                   (aget 0)
                                   (.focus)))
            (recur))))))
    om/IRenderState
    (render-state [this {:keys [terms delete-definition]}]
      (dom/div #js {:className "definition"
                    :ref (:term definition)}
        (dom/input #js {:type "text" :placeholder "Term"
                        :className "term"
                        :value (:term definition)
                        :onChange #(handle-term-change % definition)})
        (dom/button #js {:className "remove"
                         :onClick #(put! delete-definition @definition)}
                    "x")
        (dom/div #js {:className "meaning"}
          (dom/textarea #js {:className "edit"
                             :value (:meaning definition)
                             :onChange #(handle-meaning-change % definition)})
          (apply dom/pre #js {:className "bg"}
            (colorize terms
                      (:meaning definition))))))))

(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

(defn level-view [level owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete-definition (chan)})
    om/IWillMount
    (will-mount [_]
      (let [delete-definition (om/get-state owner :delete-definition)]
        (go (loop []
          (let [definition (<! delete-definition)]
            (om/transact! level
              (fn [level] (vec (remove #(= definition %) level))))
            (recur))))))
    om/IRenderState
    (render-state [this {:keys [delete-level terms delete-definition]}]
      (dom/div #js {:className "level"}
        (apply dom/div nil
          (om/build-all definition-view level
                        {:init-state {:delete-definition delete-definition}
                         :state {:terms terms}}))
        (dom/button #js {:onClick #(add-definition level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! delete-level @level)} "x")))))

(defn find-terms [levels]
  (apply concat
    (map-indexed
     (fn [idx level]
       (map #(hash-map :uuid (:uuid %)
                       :term (string/lower-case (:term %))
                       :level idx
                       :focus (:focus %))
            (remove #(empty? (:term %)) level)))
     levels)))

(defn add-level [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [app delete-level level]
  (om/build level-view level
            {:init-state {:delete-level delete-level}
             :state {:terms (find-terms (take-while #(not= % level) app))}}))

(defn handle-fullscreen-graph [owner graph-state]
  (om/set-state! owner :graph-state
    (case graph-state
      :show :fullscreen
      :hide :show
      :fullscreen :show)))

(defn handle-hide-show-graph [owner graph-state]
  (om/set-state! owner :graph-state
    (case graph-state
      :show :hide
      :fullscreen :hide
      :hide :show))
  false)

(defn app-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:graph-state :show
       :delete-level (chan)})
    om/IWillMount
    (will-mount [_]
      (let [delete-level (om/get-state owner :delete-level)]
        (go (loop []
          (let [level (<! delete-level)]
            (om/transact! app
              (fn [levels] (vec (remove #(= level %) levels))))
            (recur))))))
    om/IRenderState
    (render-state [this {:keys [graph-state
                                delete-level]}]
      (.log js/console (js/JSON.stringify (clj->js app)))
      (apply dom/div #js {:className "app"}
        (conj (mapv #(build-level app delete-level %) app)
              (dom/button #js {:className "addLevel"
                               :onClick #(add-level app)}
                          "+level")
              (dom/div #js {:className (str "graph"
                                            (when (= graph-state :fullscreen)
                                              " fullscreen"))
                            :onClick #(handle-fullscreen-graph owner graph-state)}
                (render-graph graph-state (find-terms app) app)
                (dom/button #js {:className "showHide"
                                 :onClick
                                 #(handle-hide-show-graph owner graph-state)}
                            (case graph-state
                              (:show :fullscreen) "v"
                              :hide "^"))))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "container"))})
