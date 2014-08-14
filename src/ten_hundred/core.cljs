(ns ten-hundred.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put! chan <!]]
            [cljs.core.match]
            [clojure.string :as string]
            [ten-hundred.util :refer [splice insert]]
            [ten-hundred.graph :as graph]
            [ten-hundred.terms :as terms]
            [ten-hundred.author :as author]
            [ten-hundred.docs :as docs]
            [ten-hundred.levels.definition :as definition]
            [ten-hundred.levels.level :as level]
            [ten-hundred.levels.levels :as levels]
            [cljs-uuid-utils]))

(enable-console-print!)

(defn make-initial-state []
  [[(definition/empty-definition)]])

;; whole-app view and graph pane
(defn delete-at! [levels owner data-kind source-path]
  (case data-kind
    :definition
    (let [[source-level-idx source-definition-idx] source-path]
      (om/transact! levels source-level-idx
                    #(splice % source-definition-idx))
      (om/update-state! owner :author-path
                        (fn [[author-level-idx author-definition-idx :as author-path]]
                          (if (= author-level-idx source-level-idx)
                            (cond (< author-definition-idx source-definition-idx)
                                  author-path

                                  (= author-definition-idx source-definition-idx)
                                  [0 0] ;; TODO maybe make null definition possible?

                                  (> author-definition-idx source-definition-idx)
                                  [author-level-idx (dec author-definition-idx)])

                            author-path))))

    :level
    (do (om/transact! levels #(splice % source-path))
        (om/update-state! owner :author-path
                          (fn [[author-level-idx author-definition-idx :as author-path]]
                            (cond (< author-level-idx source-path)
                                  author-path

                                  (= author-level-idx source-path)
                                  [0 0] ;; TODO

                                  (> author-level-idx source-path)
                                  [(dec author-level-idx) author-definition-idx]))))))

(defn drop! [levels owner data-kind target-path data]
  (case data-kind
    :definition
    (let [[target-level-idx target-definition-idx] target-path]
      (om/transact! levels
                    #(update-in % [target-level-idx]
                                (fn [level]
                                  (insert level target-definition-idx data))))
      (om/update-state! owner :author-path
                        (fn [[author-level-idx author-definition-idx :as author-path]]
                          (if (and (= author-level-idx target-level-idx)
                                   (>= author-definition-idx target-definition-idx))
                            [author-level-idx (inc author-definition-idx)]
                            author-path))))

    :level
    (do (om/transact! levels
                      #(insert % target-path data))
        (om/update-state! owner :author-path
                          (fn [[author-level-idx author-definition-idx :as author-path]]
                            (if (>= author-level-idx target-path)
                              [(inc author-level-idx) author-definition-idx]
                              author-path))))))

(defn define! [levels owner term]
  (let [[author-level-idx _] (om/get-state owner :author-path)

        level-idx
        (if (= 0 author-level-idx)
          (do (drop! levels owner :level 0 [])
              0)
          (dec author-level-idx))]
    (om/set-state! owner :author-path
                   [level-idx
                    (count (get @levels level-idx))])
    (om/transact! levels
                  #(update-in % [level-idx]
                              (fn [level]
                                (conj level
                                      (assoc (definition/empty-definition)
                                        :term term)))))))

(defcomponent app-view [levels owner]
  (init-state [_]
    {:mode :author

     :author-path [0 0]

     :dragging nil

     :control (chan)})

  (will-mount [_]
    (let [control (om/get-state owner :control)]
      (go-loop []
        (match (<! control)
          [:author path] (om/set-state! owner :author-path path)

          [:define term] (define! levels owner term)

          [:drag-start data-kind data source-path offset-pos mouse-pos]
          (do (om/set-state! owner :dragging {:data-kind data-kind
                                              :data data
                                              :target-path source-path
                                              :offset-pos offset-pos
                                              :mouse-pos mouse-pos})
              (delete-at! levels owner
                          data-kind source-path))

          [:drag-move mouse-x mouse-y]
          (om/set-state! owner [:dragging :mouse-pos] [mouse-x mouse-y])

          [:drag-over target-path]
          (om/set-state! owner [:dragging :target-path] target-path)

          [:drag-end]
          (let [{:keys [data-kind data target-path]} (om/get-state owner :dragging)]
            (drop! levels owner data-kind target-path data)
            (om/set-state! owner :dragging nil))

          [:delete-level level-idx]
          (delete-at! levels owner
                      :level level-idx)

          [:delete-definition path]
          (delete-at! levels owner
                      :definition path))
        (recur))))

  (render-state [this {:keys [id
                              mode
                              author-path
                              dragging
                              control]}]
    (dom/div {:class "app"}
      (dom/div {:class "topBar"}
        (dom/div {:class "tabs"}
          (dom/button {:class (if (= mode :author)
                                    "selectedMode"
                                    "")
                       :on-click #(om/set-state! owner :mode :author)}
                      (dom/i {:class "fa fa-pencil-square-o"})
                      "Authoring")
          (dom/button {:class (if (= mode :levels)
                                    "selectedMode"
                                    "")
                       :on-click #(om/set-state! owner :mode :levels)}
                      (dom/i {:class "fa fa-sitemap"})
                      "Levels"))

        (dom/div {:class "controls"}
          (dom/button {:on-click #(docs/save! id @levels)}
                      "Save to this URL"
                      (dom/i {:class "fa fa-cloud-upload"}))
          (dom/button {:on-click #(js/window.open "https://github.com/osnr/ten-hundred")}
                      "About"
                      (dom/i {:class "fa fa-info"}))))

      (let [[author-level-idx author-definition-idx] author-path]
        (dom/div {:class "viewport"
                  :style {:display (if (= mode :author)
                                     ""
                                     "none")}}
          (if-let [author-definition
                   (-> levels
                       (get author-level-idx)
                       (get author-definition-idx))]
            (om/build author/author-view author-definition
                      {:init-state {:control control}
                       :state {:level-idx author-level-idx
                               :terms (terms/find-terms
                                       (take author-level-idx levels))}})
            (dom/div {:class "authorNil"}
              "You haven't selected a definition to view here."))))

      (dom/div {:class "viewport levels"
                :style {:display (if (= mode :levels)
                                   ""
                                   "none")}}
        (om/build levels/levels-view levels
                  {:state {:control control
                           :dragging dragging
                           :author-path author-path}}))

      (om/build graph/graph-view levels
                {:init-state {:control control}
                 :state {:author-path author-path}})

      (when dragging
        (let [[mouse-x mouse-y] (:mouse-pos dragging)
              [offset-x offset-y] (:offset-pos dragging)]
          (case (:data-kind dragging)
            (om/build (case (:data-kind dragging)
                        :definition definition/definition-view
                        :level level/level-view)
                      (:data dragging)
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
                 (make-initial-state))
      (docs/load id
                 (fn [loaded-state]
                   (init-root id loaded-state))
                 (fn []
                   (init-root id (make-initial-state)))))))

(init)
