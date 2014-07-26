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
            [ten-hundred.author :as author]
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

(defn drag-clone-style [drag-clone]
  (if drag-clone
    (let [[x y] (:pos drag-clone)
          transform (str "translate(" x "px," y "px)")]
      #js {:pointer-events "none"
           :position "absolute"
           :left 0
           :top 0
           :-webkit-transform transform
           :-moz-transform transform
           :-ms-transform transform
           :transform transform })
    #js {}))

; definition view
(defn handle-term-change! [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll-top (.-scrollTop textarea))
    (om/set-state! owner :scroll-width (.-scrollWidth textarea))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn definition-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [path
                                drag-clone
                                terms
                                control
                                scroll-top scroll-width]}]
      (dom/div #js {:className "definitionWrapper"
                    :style (drag-clone-style drag-clone)

                    :onMouseDown #(drag/drag-start! control :definition @definition path %)}
        (dom/div #js {:className "definition"
                      :id (str "definition_" (string/join "_" path))}
          (dom/input #js {:type "text" :placeholder "Term"
                          :className "term"
                          :value (:term definition)
                          :onChange #(handle-term-change! % definition)})
          (dom/button #js {:className "remove"
                           :onClick #(put! control [:delete-definition path])}
                      (dom/i #js {:className "fa fa-times"}))
          (dom/div #js {:className "meaning"}
            (dom/textarea #js {:className "edit"
                               :value (:meaning definition)
                               :scrollTop scroll-top
                               :onScroll #(handle-scroll! % owner)
                               :onChange #(handle-meaning-change! % owner definition)})
            (apply dom/pre #js {:className "bg"
                                :ref "bg"
                                :scrollTop scroll-top
                                :style #js {:maxWidth scroll-width}}
              (terms/word-map #(terms/colorize-word terms %)
                              (:meaning definition)))))))))

; level view
(defn placeholder-definition-element []
  (dom/div #js {:className "placeholderDefinition"}))

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
    om/IRenderState
    (render-state [this {:keys [level-idx
                                drag-clone
                                drag-target-definition-idx
                                control terms]}]
      (dom/div #js {:className "level"
                    :id (str "level_" level-idx)
                    :style (drag-clone-style drag-clone)

                    :onMouseDown #(drag/drag-start! control :level @level level-idx %)}
        (apply dom/div
          #js {:className "passthrough"
               ;; :component js/React.DOM.div
               ;; :transitionName "defTrans"
               }
          (let [definition-elements
                (vec (map-indexed
                      (fn [definition-idx definition]
                        (om/build definition-view definition
                                  {:init-state {:control control}
                                   :state {:path [level-idx definition-idx]
                                           :terms terms}}))
                      level))]
            (if drag-target-definition-idx
              (insert definition-elements drag-target-definition-idx
                      (placeholder-definition-element))
              definition-elements)))
        (dom/button #js {:className "addDefinition"
                         :onClick #(add-definition level)} "+def")
        (dom/button #js {:className "deleteLevel"
                         :onClick #(put! control [:delete-level level-idx])} "x")))))

; whole-app view and graph pane
(defn placeholder-level-element []
  (dom/div #js {:className "placeholderLevel"}))

(defn add-level! [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [levels control
                   [drag-target-level-idx drag-target-definition-idx]
                   level-idx level]
  (om/build level-view level
            {:init-state {:control control}
             :state {:level-idx level-idx
                     :drag-target-definition-idx
                     (when (= level-idx drag-target-level-idx)
                       drag-target-definition-idx)
                     :terms (terms/find-terms (take level-idx levels))}}))

(defn log-id [x] (js/console.log (pr-str x)) x)

(defn delete-at! [levels data-kind source-path]
  (case data-kind
    :definition
    (let [[source-level-idx source-definition-idx] source-path]
      (om/transact! levels source-level-idx
                    #(splice % source-definition-idx)))

    :level
    (om/transact! levels #(splice % source-path))))

(defn drop-on [levels [target-level-idx target-definition-idx] definition]
  (update-in levels [target-level-idx]
             #(insert % target-definition-idx definition)))

(defn app-view [levels owner]
  (reify
    om/IInitState
    (init-state [_]
      {:mode :author

       :author-path [0 0]

       :dragging nil

       :control (chan)})

    om/IWillMount
    (will-mount [_]
      (let [control (om/get-state owner :control)]
        (go-loop []
          (match (<! control)
            [:author path] (om/set-state! owner :author-path path)

            [:drag-start data-kind data source-path offset-pos mouse-pos]
            (do (om/set-state! owner :dragging {:data-kind data-kind
                                                :data data
                                                :target-path source-path
                                                :offset-pos offset-pos
                                                :mouse-pos mouse-pos})
                (delete-at! levels data-kind source-path))

            [:drag-move mouse-x mouse-y]
            (om/set-state! owner [:dragging :mouse-pos] [mouse-x mouse-y])

            [:drag-over target-path]
            (om/set-state! owner [:dragging :target-path] target-path)

            [:drag-end]
            (let [{:keys [data-kind data target-path]} (om/get-state owner :dragging)]
              (case data-kind
                :definition
                (om/transact! levels #(drop-on % target-path data))
                :level
                (om/transact! levels #(insert % target-path data)))

              (om/set-state! owner :dragging nil))

            [:delete-level level-idx]
            (delete-at! levels :level level-idx)

            [:delete-definition path]
            (delete-at! levels :definition path))
          (recur))))

    om/IRenderState
    (render-state [this {:keys [id
                                mode
                                author-path
                                dragging
                                control]}]
      (dom/div #js {:className "app"}
        (dom/div #js {:className "topBar"}
          (dom/div #js {:className "tabs"}
            (dom/button #js {:className (if (= mode :author)
                                          "selectedMode"
                                          "")
                             :onClick #(om/set-state! owner :mode :author)}
                        (dom/i #js {:className "fa fa-pencil-square-o"})
                        "Authoring")
            (dom/button #js {:className (if (= mode :levels)
                                          "selectedMode"
                                          "")
                             :onClick #(om/set-state! owner :mode :levels)}
                        (dom/i #js {:className "fa fa-sitemap"})
                        "Levels"))

          (dom/div #js {:className "controls"}
            (dom/button #js {:onClick #(docs/save! id @levels)}
                        "Save to this URL"
                        (dom/i #js {:className "fa fa-cloud-upload"}))
            (dom/button #js {:onClick #(js/window.open "https://github.com/osnr/ten-hundred")}
                        "About"
                        (dom/i #js {:className "fa fa-info"}))))

        (let [[author-level-idx author-definition-idx] author-path]
          (dom/div #js {:className "viewport"
                        :style #js {:display (if (= mode :author)
                                               ""
                                               "none")}}
            (om/build author/author-view
                      (-> levels
                          (get author-level-idx)
                          (get author-definition-idx))
                      {:init-state {:control control
                                    :expand-to author-level-idx}
                       :state {:level-idx author-level-idx
                               :terms (terms/find-terms (take author-level-idx levels))}})))

        (apply dom/div #js {:className "viewport levels"
                            :style #js {:display (if (= mode :levels)
                                                   ""
                                                   "none")}}
          (conj (let [level-elements
                      (vec (map-indexed
                            #(build-level levels
                                          control
                                          (when (= (:data-kind dragging) :definition)
                                            (:target-path dragging))
                                          %1 %2)
                            levels))]
                  (if (= (:data-kind dragging) :level)
                    (insert level-elements (:target-path dragging) (placeholder-level-element))
                    level-elements))
                (dom/button #js {:className "addLevel"
                                 :onClick #(add-level! levels)}
                            "+level")))

        (om/build graph/graph-view levels
                  {:init-state {:control control}
                   :state {:author-path author-path}})

        (when dragging
          (let [[mouse-x mouse-y] (:mouse-pos dragging)
                [offset-x offset-y] (:offset-pos dragging)]
            (case (:data-kind dragging)
              (om/build (case (:data-kind dragging)
                          :definition definition-view
                          :level level-view)
                        (:data dragging)
                        {:state {:drag-clone {:pos [(- mouse-x offset-x)
                                                    (- mouse-y offset-y)]}}}))))))))

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
