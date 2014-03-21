(ns ten-hundred.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [ten-hundred.dict :as dict]))

(enable-console-print!)

(defn colorize-token [token]
  (if (re-matches #"\w+" token)
    (if (dict/words (string/lower-case token))
      (dom/span nil token)
      (dom/span #js {:className "uncommon"} token))
    token))

(defn colorize [text]
  (let [tokens (re-seq #"[^\w]+|\w+" text)]
    (println tokens)
    (map colorize-token tokens)))

(def app-state (atom {:text "Hello world!"}))

(defn handle-change [e owner state]
  (om/set-state! owner :definition (.. e -target -value)))

(defn definition-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:definition "Enter text"})
    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/input #js {:type "text" :value "Term"})
        (dom/div #js {:className "definition"}
          (apply dom/pre #js {:className "bg"}
                 (colorize (:definition state)))
          (dom/textarea #js {:className "edit"
                             :value (:definition state)
                             :onChange #(handle-change % owner state)}))))))

(om/root
  definition-view
  app-state
  {:target (. js/document (getElementById "app"))})
