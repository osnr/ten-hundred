(ns ten-hundred.edit
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]

            [clojure.string :as string]
            [goog.dom.classlist :as classlist]
            [goog.dom.selection :as selection]

            [ten-hundred.tex :as tex]
            [ten-hundred.terms :as terms]
            [ten-hundred.hover :as hover]))

(defn handle-meaning-key-press! [e owner definition] 
  (when (= 36 (.-charCode e)) ;; keypress = $
    (let [textarea (om/get-node owner "edit")
          [start end] (selection/getEndPoints textarea)

          meaning (:meaning @definition)]
      (when (and (= start end)
                 (re-matches #"[^\$]?\$" (.substr meaning (- end 2) 2))
                 (not (= "$$" (.substr meaning end 2))))
        (om/update! definition :meaning
                    (str (.substr meaning 0 end)
                         "$$$"
                         (.substr meaning end)))
        (js/setTimeout #(selection/setCursorPosition textarea (+ end 1)) 0)
        false))))

(defn handle-meaning-change! [e owner definition]
  (om/update! definition :meaning (.-value (.-target e))))

(defn handle-scroll! [e owner]
  (try
    (let [textarea (om/get-node owner "edit")
          bg (om/get-node owner "bg")]
      (set! (.-scrollTop bg) (.-scrollTop textarea))
      (when-not (= (.-scrollWidth bg) (.-scrollWidth textarea))
        (set! (.-maxWidth (.-style bg)) (.-scrollWidth textarea)))

      (when e
        (if-not (aget owner "scrollTimer")
          (classlist/add bg "scrolling")
          (do (js/clearTimeout (.-scrollTimer owner))
              (set! (.-scrollTimer owner) nil)))
        (set! (.-scrollTimer owner)
              (js/setTimeout
               (fn [_]
                 (println "stop")
                 (classlist/remove bg "scrolling")
                 (set! (.-scrollTimer owner) nil))
               250))))
    (catch js/Object e
      nil ; in case there is no bg
      )))

(defcomponent tex-span-view [data owner]
  (did-mount [_]
    (om/set-state! owner :target (om/get-node owner "math")))

  (render-state [_ {:keys [target]}]
    (dom/span {:style {:position "relative"}}

      (om/build hover/span-with-hover-view
                {:content
                 (dom/span {:class "math"
                            :ref "math"}
                   (dom/span {:class "delimiter"} "$$")
                   (:tex data)
                   (dom/span {:class "delimiter"} "$$"))

                 :hover-content
                 #(om/build tex/tex
                            {:style {}
                             :text (:tex data)})}))))

(defn colorize-tex [tex idx]
  (om/build tex-span-view {:tex tex
                           :idx idx}))

(defn colorize-image [alt src]
  (dom/span
    (om/build hover/span-with-hover-view
              {:content
               (dom/span {:class "imageTag"}
                 "!["
                 alt
                 "]("
                 src
                 ")")

               :hover-content
               (dom/img {:class "embedImage"
                         :alt alt
                         :src src})})))

(defcomponent editor-view [definition owner]
  (did-update [_ _ _]
    (handle-scroll! nil owner))

  (render-state [this {:keys [terms
                              highlight
                              path]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)]
      (dom/div {:class "meaning"}
        (dom/textarea {:class "edit"
                       :ref "edit"

                       :spell-check false

                       :placeholder "Define term here."
                       :value (:meaning definition)

                       :on-focus (when path #(put! control [:author path]))
                       :on-scroll #(handle-scroll! % owner)

                       :on-change (when-not read-only #(handle-meaning-change! % owner definition))
                       :on-key-press (when-not read-only #(handle-meaning-key-press! % owner definition))})
        (dom/pre {:class "bg"
                  :ref "bg"

                  :on-click #(terms/word-click! control (.-target %))}
          (terms/word-map {:word #(terms/colorize-word (:term definition) highlight terms %1 %2)
                           :image colorize-image
                           :tex colorize-tex}
                          (:meaning definition)))))))
