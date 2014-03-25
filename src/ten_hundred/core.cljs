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

(defn render-node [g n value]
  (let [node (.node g n)
        x (.-x value)
        y (.-y value)
        width (.-width value)
        height (.-height value)]
    (dom/g #js {:transform (str "translate(" (- x (/ width 2))
                                "," (- y (/ height 2)) ")")}
           (dom/rect #js {:className "nodeRect"
                          :width width
                          :height height})
           (dom/text #js {:className "nodeLabel"
                          :textAnchor "middle"
                          :x (/ width 2)
                          :y (/ height 2)}
                     (.-label node)))))

(defn render-edge [layout e]
  (dom/path #js {:className "dep"
                 :d (graph/points->svg-path
                     (graph/calc-points layout e))}))

(defn render-graph [terms levels]
  (let [g (->> levels
               (graph/adjacency-list terms)
               (graph/graph))
        layout (graph/layout g)
        value (.-_value layout)]
    (dom/svg #js {:width (.-width value)
                  :height (.-height value)}
      (apply dom/g #js {:className "nodes"}
             (map #(render-node g % (.node layout %))
                  (.nodes layout)))
      (apply dom/g #js {:className "edges"}
        (map #(render-edge layout %)
             (.edges layout))))))

(defn handle-term-click [term-state]
  (put! (:chan term-state) :focus))

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
   :chan (chan)})

;; (def app-state
;;   (atom [[(empty-definition)]]))

(def app-state
  (atom [[(assoc (empty-definition)
            :term "c"
            :meaning "the cool")]
         [(assoc (empty-definition)
            :term "d"
            :meaning "heh c")]]))

(defn handle-term-change [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-meaning-change [e definition]
  (om/update! definition :meaning (.. e -target -value)))

(defn definition-view [definition owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [term-chan (:chan definition)]
        (go (loop []
          (let [e (<! term-chan)]
            (cond (= e :focus) (-> (om/get-node owner)
                                   (.getElementsByClassName "edit")
                                   (aget 0)
                                   (.focus)))
            (recur))))))
    om/IRenderState
    (render-state [this state]
                  (println "rerendering" definition)
      (dom/div #js {:className "definition"
                    :ref (:term definition)}
        (dom/input #js {:type "text" :placeholder "Term"
                        :className "term"
                        :value (:term definition)
                        :onChange #(handle-term-change % definition)})
        (dom/div #js {:className "meaning"}
          (dom/textarea #js {:className "edit"
                             :value (:meaning definition)
                             :onChange #(handle-meaning-change % definition)})
          (apply dom/pre #js {:className "bg"}
            (colorize (:terms state)
                      (:meaning definition))))))))

(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

(defn level-view [level owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "level"}
        (apply dom/div nil
          (om/build-all definition-view level
                        {:state state}))
        (dom/button #js {:onClick #(add-definition level)} "+def")))))

(defn find-terms [levels]
  (apply concat
    (map-indexed
     (fn [idx level]
       (map #(hash-map :uuid (:uuid %)
                       :term (:term %)
                       :level idx
                       :chan (:chan %))
            (remove #(empty? (:term %)) level)))
     levels)))

(defn add-level [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [app level]
  (om/build level-view level
            {:state {:terms (find-terms (take-while #(not= % level) app))}}))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/div #js {:className "app"}
        (conj (mapv #(build-level app %) app)
              (dom/button #js {:className "add-level"
                               :onClick #(add-level app)}
                          "+level")
              (dom/div #js {:className "graph"}
                (render-graph (find-terms app) app)))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "container"))})
