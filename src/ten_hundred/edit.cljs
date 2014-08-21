(ns ten-hundred.edit
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]

            [clojure.string :as string]
            [goog.dom]
            [goog.dom.selection :as selection]
            [goog.dom.classlist :as classlist]

            [ten-hundred.tex :as tex]
            [ten-hundred.terms :as terms]))

(defcomponent hover-view [data owner]
  (render [this]
    (let [target (:target data)
          bounds (when target (.getBoundingClientRect target))]
      (dom/div {:class "hover"
                :style {:position "fixed"
                        :left (when bounds (.-left bounds))
                        :bottom
                        (when bounds
                          (- (.-innerHeight js/window)
                             (.-top bounds)))}}
        (:content data)))))

(defn handle-scroll! [e owner]
  (let [textarea (.-target e)
        bg (om/get-node owner "bg")]
    (om/set-state! owner :scroll {:top (.-scrollTop textarea)
                                  :width (.-scrollWidth textarea)})))

(defn handle-selection-change! [owner]
  (let [edit (om/get-node owner "edit")]
    (om/set-state! owner :selection
                   [(selection/getStart edit)
                    (selection/getEnd edit)])))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e)))
  (handle-selection-change! owner))

(defn handle-mousemove! [e owner definition terms]
  (let [target (.-target e)
        hover-state (om/get-state owner :hover-state)]
    (cond (classlist/contains target "defined")
          (when (not hover-state)
            (om/set-state! owner :hover-state
                           {:target target
                            :term (.-innerText target)}))

          :else
          (when hover-state (om/set-state! owner :hover-state nil)))))

(defcomponent tex-span-view [data owner]
  (did-mount [_]
    (om/set-state! owner :target (om/get-node owner "math")))

  (render-state [_ {:keys [target]}]
    (dom/span
      (dom/span {:class "math"
                 :ref "math"}
        (dom/span {:class "delimiter"} "$$")
        (:tex data)
        (dom/span {:class "delimiter"} "$$"))

      (om/build hover-view
                {:target target
                 :content (om/build tex/tex
                                    {:style {}
                                     :text (:tex data)})}
                {:react-key (str "hover-" (:idx data))}))))

(defn colorize-tex [tex idx]
  (om/build tex-span-view {:tex tex
                           :idx idx}))

(defcomponent editor-view [definition owner]
  (init-state [this]
    {:selection [0 0]

     :hidden false
     :hover-state nil ; TODO this should use... mixins or something?

     :scroll {:top 0
              :width nil}})

  (did-update [this prev-props prev-state]
    (let [prev-hidden (:hidden prev-state)
          hidden (om/get-state owner :hidden)]
      (when (and prev-hidden
                 (not hidden))
        (let [edit (om/get-node owner "edit")
              [start end] (om/get-state owner :selection)]
          (.focus edit)
          (selection/setStart edit start)
          (selection/setEnd edit end)))))

  (render-state [this {:keys [control terms
                              path
                              hidden
                              hover-state
                              scroll]}]
    (dom/div {:class "meaning"
              :on-mouse-move #(handle-mousemove! % owner definition terms)}
      (dom/textarea {:class "edit"
                     :ref "edit"

                     :scroll-top (:top scroll)

                     :placeholder "Define term here."
                     :value (:meaning definition)

                     :on-focus (when path #(put! control [:author path]))

                     :on-scroll #(handle-scroll! % owner)
                     :on-change #(handle-meaning-change! % owner definition)
                     :on-key-up #(handle-selection-change! owner)
                     :on-mouse-up #(handle-selection-change! owner)})
      (dom/pre {:class "bg"
                :ref "bg"

                :scroll-top (:top scroll)
                :style {:max-width (:width scroll)}

                :on-click #(terms/word-click! control (.-target %))}
        (terms/word-map #(terms/colorize-word terms %)
                        colorize-tex
                        (:meaning definition)))

      (when hover-state
        (om/build hover-view
                  {:target (:target hover-state)
                   :content (:meaning (terms/find-term terms (string/lower-case (:term hover-state))))})))))
