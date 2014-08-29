(ns ten-hundred.tex
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]))

;; port of https://github.com/Khan/react-components/blob/master/js/tex.jsx
;; w/o katex

(def pending-scripts (atom []))
(def needs-process (atom false))
(def timeout (atom nil))

(defn do-process [callback]
  (when (aget js/window "MathJax")
    (js/MathJax.Hub.Queue
     (fn []
       (let [old-element-scripts js/MathJax.Hub.elementScripts]
         (set! js/MathJax.Hub.elementScripts
               (fn [element]
                 (let [scripts (clj->js @pending-scripts)]
                   (swap! pending-scripts (constantly []))
                   (swap! needs-process (constantly false))
                   scripts)))
         (try
           (js/MathJax.Hub.Process nil callback)
           (catch js/Object e
             (throw e))
           (finally
             (set! js/MathJax.Hub.elementScripts old-element-scripts))))))))

(defn process
  ([script]
     (process script (fn [])))
  ([script callback]
     (swap! pending-scripts #(conj % script))
     (when-not @needs-process
       (swap! needs-process (constantly true))
       (swap! timeout (constantly (js/setTimeout do-process 0 callback))))))

(defn set-script-text! [owner text]
  (when-not (.-script owner)
    (let [script (js/document.createElement "script")]
      (set! (.-script owner) script)
      (set! (.-type script) "math/tex")
      (.appendChild (om/get-node owner "mathjax") script)))

  (let [script (.-script owner)]
    (if (.hasOwnProperty script "text")
      (set! (.-text script) text)
      (set! (.-textContent script) text))))

(defcomponent tex [props owner]
  (render [_]
    (dom/span {:style (:style props)}
      (dom/span {:ref "mathjax"})))

  (did-mount [_]
    (let [text (:text props)
          on-render (:on-render props)]
      (js/console.log text)
      (set-script-text! owner text)
      (process (.-script owner) on-render)))

  (did-update [this prev-props prev-state]
    (let [old-text (:text prev-props)
          new-text (:text props)
          on-render (:on-render props)

          script (.-script owner)]
      (when-not (= old-text new-text)
        (when (and script (aget js/window "MathJax"))
          (js/MathJax.Hub.Queue
           (fn []
             (if-let [jax (js/MathJax.Hub.getJaxFor script)]
               (.Text jax new-text)

               (do (set-script-text! owner new-text)
                   (process script on-render)))))))))

  (will-unmount [_]
    (let [script (.-script owner)]
      (when (and script (aget js/window "MathJax"))
        (when-let [jax (js/MathJax.Hub.getJaxFor script)]
          (.Remove jax))))))
