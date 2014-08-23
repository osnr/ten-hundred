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

(defcomponent hover-view [content owner]
  (render [this]
    (dom/div {:class "hover"
              :style {:position "absolute"}}
      content)))

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
        hover-idx (om/get-state owner :hover-idx)]
    (cond (classlist/contains target "defined")
          (when (not hover-idx)
            (om/set-state! owner :hover-idx
                           (js/parseInt (.-idx (.-dataset target)))))

          :else
          (when hover-idx (om/set-state! owner :hover-idx nil)))))

(defcomponent tex-span-view [data owner]
  (did-mount [_]
    (om/set-state! owner :target (om/get-node owner "math")))

  (render-state [_ {:keys [target]}]
    (dom/span {:style {:position "relative"}}

      (dom/span {:class "math"
                 :ref "math"}
        (dom/span {:class "delimiter"} "$$")
        (:tex data)
        (dom/span {:class "delimiter"} "$$"))

      (om/build hover-view
                (om/build tex/tex
                          {:style {}
                           :text (:tex data)})
                {:react-key (str "hover-" (:idx data))}))))

(defn colorize-tex [tex idx]
  (om/build tex-span-view {:tex tex
                           :idx idx}))

(defn colorize-word [terms hover-idx word idx]
  (dom/span {:style {:position "relative"}}
    (terms/colorize-word terms word idx)
    (when (= hover-idx idx)
      (om/build hover-view
                (:meaning (terms/find-term terms (string/lower-case word)))))))

(defcomponent editor-view [definition owner]
  (init-state [this]
    {:selection [0 0]

     :hidden false
     :hover-idx nil ; TODO this should use... mixins or something?

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
                              hover-idx
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
        (terms/word-map #(colorize-word terms hover-idx %1 %2)
                        colorize-tex
                        (:meaning definition))))))