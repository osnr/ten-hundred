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

;; expansion view
(declare expand-word)
(defn expand [control highlight terms expand-to meaning]
  (terms/word-map (if highlight
                    #(expand-word control highlight terms expand-to %)
                    identity)
                  edit/render-tex
                  meaning))

(defn expand-word [control highlight terms expand-to word]
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
          (conj (vec (expand control highlight terms expand-to term-meaning))
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

(defcomponent author-view [definition owner]
  (init-state [_]
    {:read-only false

     :hover-state nil

     :selection [0 0]})

  (will-receive-props [this next-props]
    (let [prev-props (om/get-props owner)]
      (when (not= (:term prev-props) (:term next-props))
        (om/set-state! owner :hover-state nil)
        (om/set-state! owner :expand-to nil))))

  (render-state [this {:keys [terms
                              highlight level-idx expand-to
                              hover-state]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)

          expand-to (or expand-to level-idx)]
      (dom/div {:class (str "author"
                            (when read-only " readOnly"))}
        (dom/div {:class "authorContentWrapper"}
          (dom/div {:class "authorContent"}
            ;; edit pane
            (dom/div {:class "pane editPane"
                      :style {:display (if read-only
                                         "none"
                                         "")}}
              (dom/input {:class "authorTerm"
                          :type "text" :placeholder "Term"
                          :value (:term definition)
                          :on-change #(om/update! definition :term (string/replace (.. % -target -value) #" " "_"))})

              (om/build edit/editor-view definition
                        {:state {:terms terms
                                 :highlight highlight}
                         :react-key "edit-pane-view"}))

            ;; view pane
            (dom/div {:class "pane viewPane"}
              (dom/div {:class "authorTerm"}
                (:term definition))

              (dom/div {:class "meaning"}
                (dom/div {:class "edit"
                          :key "view-pane-content"}
                  (expand control highlight terms expand-to (:meaning definition))))

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

            (when hover-state
              (om/build edit/hover-view definition
                        {:state hover-state}))))))))
