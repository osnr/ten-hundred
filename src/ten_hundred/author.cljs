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

(defn word-click! [control target]
  (cond (classlist/contains target "notDefined") ; not defined term
        (put! control [:define (.-innerText target)])

        (classlist/contains target "defined")
        (let [level-idx (js/parseInt (.-levelIdx (.-dataset target)))
              definition-idx (js/parseInt (.-definitionIdx (.-dataset target)))]
          (put! control [:author [level-idx definition-idx]]))))

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
                   :on-click #(word-click! control (.-target %))}
          "["
          lc-word
          ": "
          (conj (expand control terms expand-to term-meaning)
                "]"))
        (dom/span {:class "defined expandableWord"
                   :data-level-idx term-level-idx
                   :data-definition-idx term-definition-idx
                   :on-click #(word-click! control (.-target %))}
          word))

      (if (dict/words lc-word)
        word
        (dom/span {:class "notDefined"
                   :on-click #(word-click! control (.-target %))}
          word)))))

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

(defn flip-author-mode [author-mode]
  (case author-mode
    :view :edit
    :edit :view))

(defcomponent author-view [definition owner]
  (init-state [_]
    {:author-mode :edit
     :hover-author-mode false
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
                              author-mode hover-author-mode
                              hover-state
                              scroll-top scroll-width]}]
    (let [expand-to (or expand-to level-idx)
          author-mode (if hover-author-mode
                        (flip-author-mode author-mode)
                        author-mode)]
      (dom/div {:class "author"}
        (dom/div {:class "authorContentWrapper"}
          (dom/input {:class "authorTerm"
                      :type "text" :placeholder "Term"
                      :value (:term definition)
                      :on-change #(om/update! definition :term (string/replace (.. % -target -value) #" " "_"))})
          (dom/i {:class (str "authorMode fa fa-lg "
                              (case author-mode
                                :view "viewing fa-lock"
                                :edit "editing fa-unlock-alt"))
                  :on-mouse-enter #(om/set-state! owner :hover-author-mode true)
                  :on-mouse-leave #(om/set-state! owner :hover-author-mode false)
                  :on-click (fn [_] 
                              (om/update-state! owner :author-mode flip-author-mode)
                              (when hover-author-mode
                                (om/set-state! owner :hover-author-mode false)))})
          (dom/div {:class "authorContent"}
            ;; view mode
            (dom/div {:style {:display (case author-mode
                                         :view ""
                                         :edit "none")}}
              (dom/div {:class "authorMeaning"}
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
            (dom/div {:class "authorMeaning"
                      :style {:display (case author-mode
                                         :view "none"
                                         :edit "")}
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

                        :on-click #(word-click! control (.-target %))}
                (terms/word-map #(terms/colorize-word terms %)
                                terms/colorize-tex
                                (:meaning definition))))

            (when hover-state
              (om/build hover-view definition
                        {:state hover-state}))))))))
