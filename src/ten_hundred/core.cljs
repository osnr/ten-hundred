(ns ten-hundred.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [cljs.core.match]
            [clojure.string :as string]
            [cljs-uuid-utils :as uuid]
            [ten-hundred.dict :as dict]
            [ten-hundred.graph :as graph]
            [ten-hundred.drag :as drag]
            [ten-hundred.docs :as docs]))

(enable-console-print!)

; graph view
(defn render-node [g n value control]
  (let [node (.node g n)
        x (.-x value)
        y (.-y value)
        width (.-width value)
        height (.-height value)

        label (.-label node)]
    (dom/g #js {:transform (str "translate(" (- x (/ width 2))
                                "," (- y (/ height 2)) ")")}
           (dom/rect #js {:className "nodeRect"
                          :onClick #(do (put! control [:focus n])
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

(defn handle-term-click [control term-state]
  (put! control [:focus (:uuid term-state)]))

(defn find-term [terms token]
  (last (filter #(= (:term %) token) terms)))

(defn colorize-word [terms control word]
  (let [lc-word (string/lower-case word)]
    (cond (and terms (find-term terms lc-word))
          (dom/span #js {:className "defined"
                         :onClick #(handle-term-click
                                    control
                                    (find-term terms lc-word))}
                    word)

          (dict/words lc-word) word

          :else (dom/span #js {:className "uncommon"} word))))

(defn word-map [f text]
  (->> text
       (re-seq #"[^\w']+|[\w']+")
       (mapv #(if (re-matches #"[^\w']+" %)
                %
                (f %)))))

(defn empty-definition []
  {:uuid (uuid/uuid-string (uuid/make-random-uuid))
   :term ""
   :meaning ""})

(defn make-example-state []
  {:id (uuid/uuid-string (uuid/make-random-uuid))
   :levels [[(assoc (empty-definition)
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
               :meaning "GettysBurg")]]})

; definition expansion view
; 0 -> no expansion
; 1 -> 1 level deep
; etc
(defn expand-word [terms degree word]
  (if-let [{term-level-idx :level-idx
            term-meaning :meaning}
           (find-term terms (string/lower-case word))]
    (if (>= term-level-idx degree)
      (apply dom/span #js {:className "expanded-word"}
        "["
        (conj (expand terms degree term-meaning)
              "]"))
      (dom/span #js {:className "expandable-word"} word))
    word))

(defn expand [terms degree meaning]
  (word-map #(expand-word terms degree %) meaning))

(defn handle-degree-change [e owner]
  (js/console.log (.. e -target -value))
  (om/set-state! owner :degree (.. e -target -value)))

(defn definition-expansion-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [level-idx degree close terms]}]
      (dom/div #js {:className "expansion"
                    :onClick (constantly false)}
        (dom/input #js {:type "range"
                        :min 0
                        :max level-idx
                        :value degree
                        :onChange #(handle-degree-change % owner)})
        (apply dom/div #js {:className "expanded-meaning"}
          (expand terms degree (:meaning definition)))
        (dom/button #js {:className "close"
                         :onClick close}
                    "x")))))

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
    (render-state [this {:keys [level-idx terms
                                control
                                delete-definition]}]
      (js/console.log definition)
      (dom/div #js {:className "definition"
                    :id (:uuid definition)
                    :onMouseDown
                    (fn [e]
                      (drag/drag-start e
                                       @definition
                                       #(put! delete-definition (:uuid @definition))
                                       control))}
        (dom/button #js {:className "expand"
                         :onClick #(put! control [:inspect (:uuid @definition)])}
                    "i")
        (dom/input #js {:type "text" :placeholder "Term"
                        :className "term"
                        :value (:term definition)
                        :onChange #(handle-term-change % definition)})
        (dom/button #js {:className "remove"
                         :onClick #(put! delete-definition (:uuid @definition))}
                    "x")
        (dom/div #js {:className "meaning"}
          (dom/textarea #js {:className "edit"
                             :ref "meaningEdit"
                             :value (:meaning definition)
                             :onChange #(handle-meaning-change % owner definition)})
          (apply dom/pre #js {:className "bg"
                              :ref "meaningBg"}
            (word-map #(colorize-word terms control %)
                      (:meaning definition))))))))

; level view
(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

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
          (let [uuid (<! delete-definition)]
            (js/console.log "delete-definition" uuid)
            (om/transact! level
                          (fn [level]
                            (let [[n m] (split-with #(not= (:uuid %) uuid) level)]
                              (vec (concat n (rest m))))))
            (recur)))))
    om/IRenderState
    (render-state [this {:keys [control delete-level level-idx terms
                                delete-definition]}]
      (dom/div #js {:className "level"}
        (when (seq level)
          (apply dom/div nil ;; #js {:component js/React.DOM.div
                                 ;;      :transitionName "defTrans"}
            (om/build-all definition-view level
                          {:key :uuid
                           :init-state {:control control
                                        :delete-definition delete-definition}
                           :state {:level-idx level-idx
                                   :terms terms}})))
        (dom/button #js {:className "addDefinition"
                         :onClick #(add-definition level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! delete-level @level)} "x")))))

; whole-app view and graph pane
(defn find-terms [levels]
  (apply concat
    (map-indexed
     (fn [idx level]
       (map #(hash-map :uuid (:uuid %)
                       :term (string/lower-case (:term %))
                       :meaning (:meaning %)
                       :level-idx idx)
            (remove #(empty? (:term %)) level)))
     levels)))

(defn add-level [app]
  (om/transact! app :levels #(conj % [(empty-definition)])))

(defn build-level [app delete-level control level-idx level]
  (om/build level-view level
            {:init-state {:control control
                          :delete-level delete-level}
             :state {:level-idx level-idx
                     :terms (find-terms (take level-idx (:levels app)))}}))

(defn handle-fullscreen-graph [owner fullscreen-graph]
  (om/update-state! owner :fullscreen-graph not))

(defn log-id [x] (js/console.log (pr-str x)) x)
(defn find-definition [levels uuid]
  (->> levels
       (map-indexed
        (fn [level-idx level]
          (if-let [definition (first (filter #(= (:uuid %) uuid) level))]
            [level-idx definition]
            nil)))
       (filter identity)
       (first)))

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

(defn app-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:inspect-id nil
       :fullscreen-graph false
       :control (chan)
       :delete-level (chan)})

    om/IWillMount
    (will-mount [_]
      (let [control (om/get-state owner :control)
            delete-level (om/get-state owner :delete-level)]
        (go-loop []
          (match (<! control)
            [:focus id] (-> js/document
                         (.getElementById id)
                         (.getElementsByClassName "edit")
                         (aget 0)
                         (.focus))
            [:inspect id] (om/set-state! owner :inspect-id id)
            [:drop-on definition position dest-uuid]
            (om/transact! app :levels #(drop-on % definition position dest-uuid))
            [:drop-on-level definition level-idx]
            (om/transact! app :levels #(drop-on-level % definition level-idx)))
          (recur))

        (go-loop []
          (let [level (<! delete-level)]
            (om/transact! app :levels
                          (fn [levels] (vec (remove #(= level %) levels))))
            (recur)))))

    om/IRenderState
    (render-state [this {:keys [inspect-id
                                control
                                fullscreen-graph
                                delete-level]}]
      (dom/div #js {:className "app"}
        (dom/div #js {:className "levelsWrapper"}
          (apply dom/div #js {:className "levels"}
                 (conj (vec (map-indexed #(build-level app delete-level control
                                                       %1 %2)
                                         (:levels app)))
                       (dom/button #js {:className "addLevel"
                                        :onClick #(add-level app)}
                                   "+level"))))

        (dom/div #js {:className "sidebar"}
          (when inspect-id
            (let [[level-idx definition] (find-definition (:levels app) inspect-id)]
              (om/build definition-expansion-view
                        definition
                        {:react-key (str (:uuid definition) "_expansion")
                         :init-state {:degree level-idx
                                      :close #(om/set-state! owner :inspect-id nil)}
                         :state {:level-idx level-idx
                                 :terms (find-terms (take level-idx (:levels app)))}})))

          (dom/div #js {:className (str "graph"
                                        (when fullscreen-graph
                                          " fullscreen"))
                        :onClick #(handle-fullscreen-graph owner fullscreen-graph)}
            (render-graph fullscreen-graph
                          (find-terms (:levels app))
                          (:levels app)
                          control)
            (dom/div #js {:className "controls"}
              (dom/button #js {:onClick 
                               (fn [] 
                                 (docs/save! @app)
                                 false)}
                          "save")
              (dom/button #js {:onClick
                               (fn []
                                 (js/window.open "https://github.com/osnr/ten-hundred")
                                 false)}
                          "about"))))))))

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
