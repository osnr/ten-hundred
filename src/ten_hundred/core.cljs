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
            [ten-hundred.drag :as drag]
            [ten-hundred.docs :as docs]
            [cljs-uuid-utils]))

(enable-console-print!)

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
                                drag-clone
                                terms
                                control
                                delete-definition]}]
      (dom/div #js {:className "definition"
                    :id (str "definition_" (string/join "_" path))
                    :style (if drag-clone
                             (let [[x y] (:pos drag-clone)]
                               #js {:pointer-events "none"
                                    :position "absolute"
                                    :left 0
                                    :top 0
                                    :transform (str "translate(" x "px," y "px)")})
                             #js {})

                    :onMouseDown #(drag/definition-drag-start! control @definition path %)}
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
(defn placeholder-element []
  (dom/div #js {:className "placeholder"}))

(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

(defn splice [v idx]
  (vec (concat
        (subvec v 0 idx)
        (subvec v (inc idx)))))

(defn insert [v idx itm]
  (vec (concat
        (subvec v 0 idx)
        [itm]
        (subvec v idx))))

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
                                drag-target-definition-idx
                                control delete-level terms
                                delete-definition]}]
      (dom/div #js {:className "level"
                    :id (str "level_" level-idx)}
        (apply dom/div nil ;; #js {:component js/React.DOM.div
                           ;;      :transitionName "defTrans"}
               (let [definition-elements
                     (vec (map-indexed (fn [definition-idx definition]
                                         (om/build definition-view definition
                                                   {:init-state {:control control
                                                                 :delete-definition delete-definition}
                                                    :state {:path [level-idx definition-idx]
                                                            :terms terms}}))
                                       level))]
                 (if drag-target-definition-idx
                   (insert definition-elements drag-target-definition-idx
                           (placeholder-element))
                   definition-elements)))
        (dom/button #js {:className "addDefinition"
                         :onClick #(add-definition level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! delete-level @level)} "x")))))

; whole-app view and graph pane
(defn add-level! [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [levels delete-level control
                   [drag-target-level-idx drag-target-definition-idx]
                   level-idx level]
  (om/build level-view level
            {:init-state {:control control
                          :delete-level delete-level}
             :state {:level-idx level-idx
                     :drag-target-definition-idx
                     (when (= level-idx drag-target-level-idx)
                       drag-target-definition-idx)
                     :terms (terms/find-terms (take level-idx levels))}}))

(defn handle-fullscreen-graph [owner fullscreen-graph]
  (om/update-state! owner :fullscreen-graph not))

(defn log-id [x] (js/console.log (pr-str x)) x)

(defn delete-drag-source [levels [source-level-idx source-definition-idx]]
  (update-in levels [source-level-idx]
             #(splice % source-definition-idx)))

(defn drop-on [levels definition [target-level-idx target-definition-idx]]
  (update-in levels [target-level-idx]
             #(insert % target-definition-idx definition)))

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

            [:drag-start :definition definition source-path offset-pos mouse-pos]
            (do (om/set-state! owner :dragging {:data definition
                                                :target-path source-path
                                                :offset-pos offset-pos
                                                :mouse-pos mouse-pos})
                (om/transact! levels #(delete-drag-source % source-path)))

            [:drag-move mouse-x mouse-y]
            (om/set-state! owner [:dragging :mouse-pos] [mouse-x mouse-y])

            [:drag-over :definition target-path]
            (do (println "drag-over" target-path)
                (om/set-state! owner [:dragging :target-path] target-path))

            [:drag-end]
            (do (js/console.log "dragend")
                (om/transact! levels #(drop-on %
                                               (om/get-state owner [:dragging :data])
                                               (om/get-state owner [:dragging :target-path])))
                (om/set-state! owner :dragging nil))

            [:drag-over :definition :level target-idx]
            (js/console.log "drag ovah")

            [:drag-end :definition :level target-idx]
            (om/transact! levels #(drop-on-level % (om/get-state owner :dragging)) target-idx))
          (recur))

        (go-loop []
          (let [level (<! delete-level)]
            (om/transact! levels
                          (fn [levels] (vec (remove #(= level %) levels))))
            (recur)))))

    om/IRenderState
    (render-state [this {:keys [id
                                inspect-path
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
                                                       (:target-path dragging)
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
                        {:init-state {:expand-to inspect-level-idx
                                      :close #(om/set-state! owner :inspect-path nil)}
                         :state {:level-idx inspect-level-idx
                                 :terms (terms/find-terms (take inspect-level-idx levels))}})))

          (dom/div #js {:className (str "graph"
                                        (when fullscreen-graph
                                          " fullscreen"))
                        :onClick #(handle-fullscreen-graph owner fullscreen-graph)}
            (graph/render-graph fullscreen-graph
                                (terms/find-terms levels)
                                levels
                                control)))

        (dom/div #js {:className "controls"}
          (dom/button #js {:onClick 
                           (fn []
                             (docs/save! id @levels)
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
                        "v")))

        (when dragging
          (let [[mouse-x mouse-y] (:mouse-pos dragging)
                [offset-x offset-y] (:offset-pos dragging)]
            (om/build definition-view (:data dragging)
                      {:state {:drag-clone {:pos [(- mouse-x offset-x)
                                                  (- mouse-y offset-y)]}}})))))))

(defn init-root [id app-state]
  (js/window.history.replaceState "" ""
                                  (str "#/" id))
  (js/console.log (pr-str app-state))
  (om/root app-view
           (atom app-state)
           {:target (. js/document (getElementById "container"))
            :init-state {:id id}}))

(defn init []
  (let [id (second (string/split (.-URL js/document) #"#/"))]
    (if (empty? id)
      (init-root (cljs-uuid-utils/uuid-string (cljs-uuid-utils/make-random-uuid))
                 (make-example-state))
      (docs/load id
                 (fn [loaded-state]
                   (init-root id loaded-state))
                 (fn []
                   (init-root id (make-example-state)))))))

(init)
