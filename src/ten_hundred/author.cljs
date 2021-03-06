(ns ten-hundred.author
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [goog.dom.selection :as selection]

            [cljs.core.async :refer [put!]]
            [clojure.string :as string]

            [ten-hundred.terms :as terms]
            [ten-hundred.dict :as dict]
            [ten-hundred.tex :as tex]
            [ten-hundred.edit :as edit]
            [ten-hundred.hover :as hover]))

;; expansion view
(declare expand-word)
(defn expand [owner control terms highlight expand-to meaning]
  (terms/word-map {:word #(expand-word owner control terms highlight expand-to %)
                   :image #(dom/img {:class "embedImage" :alt %1 :src %2})
                   :tex #(om/build tex/tex {:text %})}
                  meaning))

(defn expand-word [owner control terms highlight expand-to word]
  (let [lc-word (string/lower-case word)]
    (if-let [{[term-level-idx term-definition-idx :as term-path] :path
              term-meaning :meaning}
             (terms/find-term terms lc-word)]
      (if (>= term-level-idx expand-to)
        (dom/span {:class "defined expandedWord"
                   :data-level-idx term-level-idx
                   :data-definition-idx term-definition-idx
                   :on-click #(terms/word-click! control (.-target %))}
          "["
          lc-word
          ": "
          (conj (vec (expand owner control terms highlight expand-to term-meaning))
                "]"))
        (dom/span {:class "defined expandableWord"

                   :data-level-idx term-level-idx
                   :data-definition-idx term-definition-idx

                   :on-click #(terms/word-click! control (.-target %))}
          (om/build hover/span-with-hover-view
                    {:content word
                     ;; FIXME code duplication w/ colorize-word
                     :hover-style {:width (min 400 (+ 20 (* 7 (count term-meaning))))}
                     :hover-content term-meaning})))

      (cond (dict/words lc-word) word

            highlight
            (dom/span {:class "notDefined"
                       :on-click #(terms/word-click! control (.-target %))}
              word)

            :else
            word))))

(defn handle-expand-to-change! [e owner]
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defcomponent author-view [definition owner]
  (init-state [_]
    {:minimize-edit false
     :minimize-view true})

  (will-receive-props [this next-props]
    (let [prev-props (om/get-props owner)]
      (when (not= (:term prev-props) (:term next-props))
        (om/set-state! owner :expand-to nil))))

  (render-state [this {:keys [terms
                              highlight level-idx expand-to
                              minimize-edit minimize-view]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)

          expand-to (or expand-to level-idx)]
      (dom/div {:class (str "author"
                            (when read-only " readOnly")
                            (when (or minimize-edit minimize-view) " minimizing"))}
        (when minimize-edit
          (dom/button {:class "unminimize unminimizeEdit"
                       :on-click #(om/set-state! owner :minimize-edit false)}
                      (dom/i {:class "fa fa-angle-right"})))
        (when (and minimize-view (not read-only))
          (dom/button {:class "unminimize unminimizeView"
                       :on-click #(om/set-state! owner :minimize-view false)}
                      (dom/i {:class "fa fa-angle-left"})))

        (dom/div {:class "authorContentWrapper"}
          (dom/div {:class "authorContent"}
            ;; edit pane
            (when-not minimize-edit
              (dom/div {:class "pane editPane"
                        :style {:display (if read-only
                                           "none"
                                           "")}}
                (dom/input {:class "authorTerm"
                            :type "text" :placeholder "Term"
                            :value (:term definition)
                            :on-change #(om/update! definition :term (terms/escape-term (.. % -target -value)))})

                (dom/input {:class "authorSynonyms"
                            :type "text"
                            :ref "synonyms"

                            :on-mouse-enter #(set! (.-placeholder (om/get-node owner "synonyms"))
                                                   "synonym_1 synonym_2 ...")
                            :on-mouse-leave #(set! (.-placeholder (om/get-node owner "synonyms")) "")

                            :value (string/join " " (:synonyms definition))
                            :on-change #(om/update! definition :synonyms
                                                    (terms/get-synonyms (.. % -target -value)))})

                (dom/button {:class "minimize minimizeEdit"
                             :on-click #(om/set-state! owner :minimize-edit true)}
                            (dom/i {:class "fa fa-angle-left"}))

                (dom/button {:class "authorDelete"
                             :on-click #(put! control [:delete-definition :author])}
                            (dom/i {:class "fa fa-times"}))

                (om/build edit/editor-view definition
                          {:state {:terms terms
                                   :highlight highlight}
                           :react-key "edit-pane-view"})))

            ;; view pane
            (when (or read-only (not minimize-view))
              (dom/div {:class "pane viewPane"}
                (dom/div {:class "authorTerm"}
                  (:term definition))

                (when-not read-only
                  (dom/button {:class "minimize minimizeView"
                               :on-click #(om/set-state! owner :minimize-view true)}
                              (dom/i {:class "fa fa-angle-right"})))

                (dom/div {:class "meaning"}
                  (dom/div {:class "edit"
                            :key "view-pane-content"}
                    (expand owner control terms highlight expand-to (:meaning definition))))

                (dom/div {:class (str "expandControls"
                                      (when (= level-idx 0)
                                        " disabled"))}
                  (dom/i {:class "fa fa-plus-square"})
                  (dom/input {:class "expandSlider"
                              :type "range"
                              :disabled (= level-idx 0)
                              :min 0
                              :max level-idx
                              :value (or expand-to level-idx)
                              :on-change #(handle-expand-to-change! % owner)})
                  (dom/i {:class "fa fa-minus-square"}))))))))))
