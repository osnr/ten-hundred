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
            [ten-hundred.edit :as edit]))

(defn render-tex [tex]
  (om/build tex/tex {:style {}
                     :text tex}))

;; expansion view
(declare expand-word)
(defn expand [control terms expand-to meaning]
  (terms/word-map #(expand-word control terms expand-to %) render-tex meaning))

(defn expand-word [control terms expand-to word]
  (let [lc-word (string/lower-case word)]
    (if-let [{[term-level-idx term-definition-idx] :path
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
          (conj (vec (expand control terms expand-to term-meaning))
                "]"))
        (dom/span {:class "defined expandableWord"
                   :data-level-idx term-level-idx
                   :data-definition-idx term-definition-idx
                   :on-click #(terms/word-click! control (.-target %))}
          word))

      (if (dict/words lc-word)
        word
        (dom/span {:class "notDefined"
                   :on-click #(terms/word-click! control (.-target %))}
          word)))))

(defn handle-expand-to-change! [e owner]
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defn flip-author-mode! [owner]
  (om/update-state! owner :author-mode
                    (fn [author-mode]
                      (case author-mode
                        :view :edit
                        :edit :view))))

(defcomponent author-view [definition owner]
  (init-state [_]
    {:read-only false

     :author-mode :edit
     :hover-author-mode false

     :hover-state nil

     :selection [0 0]})

  (will-receive-props [this next-props]
    (let [prev-props (om/get-props owner)]
      (when (not= (:term prev-props) (:term next-props))
        (om/set-state! owner :hover-state nil)
        (om/set-state! owner :expand-to nil))))

  (render-state [this {:keys [level-idx expand-to close terms
                              hover-state
                              author-mode hover-author-mode]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)

          author-mode (if read-only :view author-mode)
          expand-to (or expand-to level-idx)]
      (dom/div {:class "author"}
        (dom/div {:class "authorContentWrapper"}
          (dom/input {:class "authorTerm"
                      :type "text" :placeholder "Term"
                      :value (:term definition)
                      :on-change #(om/update! definition :term (string/replace (.. % -target -value) #" " "_"))})
          (dom/button (merge
                       {:class (str "authorMode "
                                    (when hover-author-mode
                                      "hoverCausedModeChange ")
                                    (case author-mode
                                      :view "viewing"
                                      :edit "editing"))}
                       (if read-only
                         {}
                         {:on-mouse-enter (fn [_]
                                            (flip-author-mode! owner)
                                            (om/set-state! owner :hover-author-mode true))
                          :on-mouse-leave (fn [_]
                                            (when hover-author-mode
                                              (flip-author-mode! owner))
                                            (om/set-state! owner :hover-author-mode false))
                          :on-click (fn [_]
                                      (if hover-author-mode
                                        (om/set-state! owner :hover-author-mode false)
                                        (flip-author-mode! owner)))}))
                      (if read-only
                        "Read-only"
                        (case author-mode
                          :view "Viewing"
                          :edit "Editing")))

          (dom/div {:class "authorContent"}
            ;; view mode
            (dom/div {:style {:display (case author-mode
                                         :view ""
                                         :edit "none")}}
              (dom/div {:class "meaning"}
                (dom/div {:class "edit"}
                  (expand control terms expand-to (:meaning definition))))

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
                (dom/i {:class "fa fa-minus-square"})))

            ;; edit mode
            (dom/div {:style {:display (case author-mode
                                         :view "none"
                                         :edit "")}}
              (om/build edit/editor-view definition
                        {:state {:terms terms

                                 :hidden (= author-mode :view)}}))

            (when hover-state
              (om/build edit/hover-view definition
                        {:state hover-state}))))))))
