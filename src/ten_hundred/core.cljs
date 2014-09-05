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
            [ten-hundred.notifications :as notifications]
            [ten-hundred.terms :as terms]
            [ten-hundred.author :as author]
            [ten-hundred.docs :as docs]
            [ten-hundred.levels.definition :as definition]
            [ten-hundred.levels.level :as level]
            [ten-hundred.levels.levels :as levels]))

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
      (om/update-state!
       owner :author-path
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
        (om/update-state!
         owner :author-path
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
      (om/transact!
       levels
       #(update-in % [target-level-idx]
                   (fn [level]
                     (insert level target-definition-idx data))))
      (om/update-state!
       owner :author-path
       (fn [[author-level-idx author-definition-idx :as author-path]]
         (if (and (= author-level-idx target-level-idx)
                  (>= author-definition-idx target-definition-idx))
           [author-level-idx (inc author-definition-idx)]
           author-path))))

    :level
    (do (om/transact! levels
                      #(insert % target-path data))
        (om/update-state!
         owner :author-path
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

(defn open-disk! [levels files]
  (docs/open-disk!
   files
   (fn [new-levels]
     (om/transact! levels #(into new-levels %)))))

(defn notify! [notifications notification]
  (om/transact! notifications #(conj % notification)))

(defn save! [owner notifications file levels]
  (om/set-state! owner :dirty false)
  (docs/save! file levels
              (fn [new-file]
                (let [suffix (str "?/" (docs/id new-file))]
                  (js/window.history.replaceState "" "" suffix)
                  (when-not file
                    (notify! notifications
                             [:saved (str (docs/base-location) suffix)]))
                  (om/set-state! owner :file new-file)))

              (fn [error]
                (js/alert error))))

(defn publish! [notifications file levels]
  (docs/publish! file levels
                 (fn [publish-url]
                   (notify! notifications
                            [:published publish-url
                             (str (docs/base-location) "?/" (docs/id file))]))

                 (fn [error]
                   (js/alert error))))

(defcomponent app-view [app owner]
  (init-state [_]
    {:dirty false

     :mode :author

     :highlight false

     :dragging nil})

  (will-receive-props [this next-props]
    (when-not (= (:levels next-props) (:levels (om/get-props owner)))
      (om/set-state! owner :dirty true)))

  (will-mount [_]
    (let [control (om/get-shared owner :control)
          read-only (om/get-shared owner :read-only)

          levels (:levels app)]
      (go-loop []
        (match (<! control)
          [:author path]
          (om/set-state! owner :author-path path)

          [:define term]
          (when-not read-only
            (define! levels owner term))

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

          [:delete-definition :author]
          (delete-at! levels owner
                      :definition
                      (om/get-state owner :author-path))

          [:delete-definition path]
          (delete-at! levels owner
                      :definition path))
        (recur))))

  (render-state [this {:keys [file
                              mode
                              highlight
                              author-path
                              notifications
                              dragging]}]
    (let [read-only (om/get-shared owner :read-only)

          levels (:levels app)
          notifications (:notifications app)]
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
            (dom/label
             (dom/input {:type "checkbox"
                         :checked highlight
                         :on-change #(om/set-state! owner :highlight (.-checked (.-target %)))})
             "Flag unknown words")

            (dom/button {:on-click #(js/window.open (docs/base-location))}
                        "New"
                        (dom/i {:class "fa fa-file"}))

            (if read-only
              [(dom/button {:on-click #(docs/save-disk! @levels)}
                           "Export file"
                           (dom/i {:class "fa fa-floppy-o"}))
               (dom/button {:on-click #(save! owner notifications file @levels)}
                           "Save to new link"
                           (dom/i {:class "fa fa-code-fork"}))]

              [(dom/input {:type "file"
                           :ref "fileChooser"
                           :style {:display "none"}
                           :multiple true
                           :on-change #(open-disk! levels (prim-seq (.-files (.-target %))))})

               (dom/button {:on-click #(.click (om/get-node owner "fileChooser"))}
                           "Import file here"
                           (dom/i {:class "fa fa-folder-open"}))
               (dom/button {:on-click #(docs/save-disk! @levels)}
                           "Export file"
                           (dom/i {:class "fa fa-floppy-o"}))
               (dom/button {:on-click #(save! owner notifications file @levels)}
                           "Save to Web"
                           (dom/i {:class "fa fa-cloud-upload"}))
               (dom/button {:on-click #(publish! notifications file @levels)}
                           "Publish"
                           (dom/i {:class "fa fa-globe"}))])

            (dom/button {:on-click #(js/window.open "https://github.com/osnr/ten-hundred")}
                        "About"
                        (dom/i {:class "fa fa-info"}))))

        (case mode
          :author
          (let [[author-level-idx author-definition-idx] author-path]
            (dom/div {:class "viewport"}
              (if-let [author-definition
                       (-> levels
                           (get author-level-idx)
                           (get author-definition-idx))]
                (om/build author/author-view author-definition
                          {:state {:level-idx author-level-idx
                                   :highlight highlight
                                   :terms (terms/find-terms
                                           (take author-level-idx levels))}})
                (dom/div {:class "authorNil"}
                  "You haven't selected a definition to view here."))))

          :levels
          (dom/div {:class "viewport levels"}
            (om/build levels/levels-view levels
                      {:state {:highlight highlight
                               :dragging dragging
                               :author-path author-path}})))

        (om/build graph/graph-view levels
                  {:state {:author-path author-path}})

        (om/build notifications/notifications-view notifications)

        (when dragging
          (let [[mouse-x mouse-y] (:mouse-pos dragging)
                [offset-x offset-y] (:offset-pos dragging)]
            (case (:data-kind dragging)
              (om/build (case (:data-kind dragging)
                          :definition definition/definition-view
                          :level level/level-view)
                        (:data dragging)
                        {:react-key "drag-preview"
                         :state {:drag-clone {:pos [(- mouse-x offset-x)
                                                    (- mouse-y offset-y)]}}})))))))

  (did-update [_ _ _]
    (set! (.-title js/document)
          (str (when (om/get-state owner :dirty)
                 "(unsaved) ")
               (:term (get-in (:levels app) (om/get-state owner :author-path)))
               " – ten-hundred"))
    (set! (.-onbeforeunload js/window)
          (if (om/get-state owner :dirty)
            (fn []
              (str "You haven't saved your document to an online link. "
                   "If you close this window, you might lose your data."))
            nil))))

(defn init-root
  ([]
     (init-root nil (make-initial-state) false))
  ([file levels read-only]
     (when-not read-only
       (js/window.history.replaceState "" ""
                                       (if file
                                         (str "?/" (docs/id file))
                                         "")))
     (js/console.log (pr-str levels))
     (om/root app-view
              (atom {:levels levels
                     :notifications []})
              {:target (js/document.getElementById "container")
               :init-state {:file file
                            :author-path (terms/top-definition-path levels)}

               :shared {:control (chan)
                        :read-only read-only}})))

(defn init []
  (docs/init!)
  (let [id (.substring js/window.location.search 2)]
    (cond (empty? id)
          (init-root)

          (= (.substring id 0 5) "view/")
          (let [publish-id (.substring id 5)]
            (docs/load-publish publish-id
                               (fn [loaded-levels]
                                 (init-root nil loaded-levels true))
                               (fn [error]
                                 (js/alert error)
                                 (init-root))))

          :else
          (docs/load id
                     (fn [file loaded-levels]
                       (init-root file loaded-levels false))
                     (fn [error]
                       (js/alert error)
                       (init-root))))))

(init)
