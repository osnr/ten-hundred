(ns ten-hundred.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]
            [ten-hundred.dict :as dict]))

(enable-console-print!)

(defn handle-term-click [term-state]
  (println term-state)
  (put! (:chan term-state) :focus))

(defn colorize-token [terms token]
  (cond (re-matches #"[^\w']+" token) token

        (and terms (terms token))
        (dom/span #js {:className "term"
                       :onClick #(handle-term-click (terms token))} token)

        (dict/words (string/lower-case token)) token

        :else (dom/span #js {:className "uncommon"} token)))

(defn colorize [terms text]
  (->> text
       (re-seq #"[^\w']+|[\w']+")
       (map #(colorize-token terms %))))

(defn empty-definition []
  {:term ""
   :meaning ""
   :chan (chan)})

(def app-state
  (atom [[(empty-definition)]]))

(defn handle-term-change [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-meaning-change [e definition]
  (om/update! definition :meaning (.. e -target -value)))

(defn definition-view [definition owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "definition"
                    :ref (:term definition)}
        (dom/input #js {:type "text" :placeholder "Term"
                        :value (:term definition)
                        :onChange #(handle-term-change % definition)})
        (dom/div #js {:className "meaning"}
          (dom/textarea #js {:className "edit"
                             :value (:meaning definition)
                             :onChange #(handle-meaning-change % definition)})
          (apply dom/pre #js {:className "bg"}
            (colorize (:terms state)
                      (:meaning definition))))))))

(defn add-definition [level]
  (om/transact! level #(conj % (empty-definition))))

(defn level-view [level owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "level"}
        (apply dom/div nil
          (om/build-all definition-view level
                        {:state state}))
        (dom/button #js {:onClick #(add-definition level)} "+def")))))

(defn find-terms [levels]
  (apply merge
         (map-indexed (fn [idx level]
                        (apply merge
                               (map #(hash-map (:term %)
                                               {:level idx
                                                :chan (:chan %)})
                                    (remove #(empty? (:term %)) level))))
                      levels)))

(defn add-level [app]
  (om/transact! app #(conj % [(empty-definition)])))

(defn build-level [app level]
  (om/build level-view level
            {:state {:terms (find-terms (take-while #(not= % level) app))}}))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/div {:className "app"}
        (conj (mapv #(build-level app %) app)
              (dom/button #js {:className "add-level"
                               :onClick #(add-level app)}
                          "+level"))))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "container"))})
