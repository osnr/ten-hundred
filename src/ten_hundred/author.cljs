(ns ten-hundred.author
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [ten-hundred.terms :as terms]
            [ten-hundred.dict :as dict]
            [ten-hundred.tex :as tex]
            [goog.dom.classlist :as classlist]))

(defcomponent hover-view [definition owner]
  (render-state [this {:keys [terms term left bottom]}]
    (dom/div {:class "hover"
              :style {:position "fixed"
                      :left left
                      :bottom bottom}}
      (:meaning (terms/find-term terms (string/lower-case term))))))

(defn render-tex [tex]
  (om/build tex/tex {:style {}
                     :text tex}))

;; expansion view
(declare expand-word)
(defn expand [terms expand-to meaning]
  (terms/word-map #(expand-word terms expand-to %) render-tex meaning))

(defn expand-word [terms expand-to word]
  (let [lc-word (string/lower-case word)]
    (if-let [{[term-level-idx _] :path
              term-meaning :meaning}
             (terms/find-term terms lc-word)]
      (if (>= term-level-idx expand-to)
        (dom/span {:class "expanded-word"}
          "["
          (conj (expand terms expand-to term-meaning)
                "]"))
        (dom/span {:class "expandable-word"} word))

      (if (dict/words lc-word)
        word
        (dom/span {:class "notDefined"} word)))))

(defn handle-expand-to-change! [e owner]
  (om/set-state! owner :expand-to (js/parseInt (.. e -target -value))))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll-top (.-scrollTop textarea))
    (om/set-state! owner :scroll-width (.-scrollWidth textarea))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn handle-mousemove! [e owner definition terms]
  (let [target (.-target e)
        hover-state (om/get-state owner :hover-state)]
    (cond (classlist/contains target "defined")
          (when (not hover-state)
            (let [bounds (.getBoundingClientRect target)]
              (om/set-state! owner :hover-state
                             {:terms terms
                              :term (.-innerText target)

                              :left (.-left bounds)
                              :bottom
                              (- (.-innerHeight js/window)
                                 (.-top bounds))

                              :alpha false})))

          :else
          (when hover-state (om/set-state! owner :hover-state nil)))))

(defn handle-bg-click! [e owner control]
  (let [target (.-target e)]
    (cond (classlist/contains target "notDefined") ; not defined term
          (put! control [:define (.-innerText target)])

          (classlist/contains target "defined")
          (let [level-idx (js/parseInt (.-levelIdx (.-dataset target)))
                definition-idx (js/parseInt (.-definitionIdx (.-dataset target)))]
            (put! control [:author [level-idx definition-idx]])))))

(defcomponent author-view [definition owner]
  (init-state [_]
    {:author-mode :view
     :hover-state nil})

  (will-receive-props [this next-props]
    (let [prev-props (om/get-props owner)]
      (when (not= (:term prev-props) (:term next-props))
        (om/set-state! owner :hover-state nil)
        (om/set-state! owner :expand-to nil))))

  (did-update [this prev-props prev-state]
    (let [prev-author-mode (:author-mode prev-state)
          author-mode (om/get-state owner :author-mode)]
      (when (and (= prev-author-mode :view) (= author-mode :edit))
        (.focus (om/get-node owner "edit")))))

  (render-state [this {:keys [control level-idx expand-to close terms
                              author-mode
                              hover-state
                              scroll-top scroll-width]}]
    (let [expand-to (or expand-to level-idx)]
      (dom/div {:class "author"}
        (dom/div {:class "authorContentWrapper"}
          (dom/input {:class "authorTerm"
                      :type "text" :placeholder "Term"
                      :value (:term definition)
                      :on-change #(om/update! definition :term (string/replace (.. % -target -value) #" " "_"))})
          (dom/div {:class "authorContent"}
            ;; view mode
            (dom/div {:style {:display (case author-mode
                                         :view ""
                                         :edit "none")}}
              (dom/div {:class "authorMeaning"
                        :on-click #(om/set-state! owner :author-mode :edit)}
                (dom/div {:class "edit"}
                  (expand terms expand-to (:meaning definition))))

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
            (dom/div {:class "authorMeaning"
                      :style {:display (case author-mode
                                         :view "none"
                                         :edit "")}
                      :on-blur #(om/set-state! owner :author-mode :view)
                      :on-mouse-move #(handle-mousemove! % owner definition terms)}
              (dom/textarea {:class "edit"
                             :ref "edit"

                             :scroll-top scroll-top

                             :placeholder "Define term here."
                             :value (:meaning definition)
                             :on-scroll #(handle-scroll! % owner)
                             :on-change #(handle-meaning-change! % owner definition)})
              (dom/pre {:class "bg"
                        :ref "bg"

                        :scroll-top scroll-top
                        :style {:max-width scroll-width}

                        :on-click #(handle-bg-click! % owner control)}
                (terms/word-map #(terms/colorize-word terms %)
                                terms/colorize-tex
                                (:meaning definition))))

            (when hover-state
              (om/build hover-view definition
                        {:state hover-state}))))))))
