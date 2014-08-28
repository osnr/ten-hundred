(ns ten-hundred.edit
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]

            [clojure.string :as string]
            [goog.dom.classlist :as classlist]
            [goog.dom.selection :as selection]

            [ten-hundred.tex :as tex]
            [ten-hundred.terms :as terms]))

(defn render-tex [tex]
  (om/build tex/tex {:style {}
                     :text tex}))

(defcomponent hover-view [content owner]
  (render [this]
    (dom/div {:class "hover"
              :style (when (string? content)
                       {:width (min 400 (+ 20 (* 7 (count content))))})}
      (terms/word-map identity render-tex content))))

(defcomponent tex-hover-view [content owner]
  (render [this]
    (dom/div {:class "hover texHover"}
      content)))

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

      (om/build tex-hover-view
                (om/build tex/tex
                          {:style {}
                           :text (:tex data)})
                {:react-key (str "hover-" (:idx data))}))))

(defn colorize-tex [tex idx]
  (om/build tex-span-view {:tex tex
                           :idx idx}))

(defn colorize-word [self-term highlight terms hover-idx word idx]
  (dom/span {:style {:position "relative"}}
    (terms/colorize-word self-term highlight terms word idx)
    (when (= hover-idx idx)
      (om/build hover-view
                (:meaning (terms/find-term terms (string/lower-case word)))))))

(defcomponent editor-view [definition owner]
  (init-state [this]
    {:hover-idx nil ; TODO this should use... mixins or something?
     })

  (did-update [_ _ _]
    (handle-scroll! nil owner))

  (render-state [this {:keys [terms
                              highlight
                              path
                              hover-idx]}]
    (let [read-only (om/get-shared owner :read-only)
          control (om/get-shared owner :control)]
      (dom/div {:class "meaning"
                :on-mouse-move #(handle-mousemove! % owner definition terms)}
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
          (terms/word-map #(colorize-word (:term definition) highlight terms hover-idx %1 %2)
                          colorize-tex
                          (:meaning definition)))))))
