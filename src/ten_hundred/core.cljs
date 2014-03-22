(ns ten-hundred.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]
            [ten-hundred.dict :as dict]))

(enable-console-print!)

(defn colorize-token [token]
  (if (re-matches #"[\w']+" token)
    (if (dict/words (string/lower-case token))
      (dom/span nil token)
      (dom/span #js {:className "uncommon"} token))
    token))

(defn colorize [text]
  (->> text
       (re-seq #"[^\w']+|[\w']+")
       (map colorize-token)))

(def app-state
  (atom [[{:term ""
           :meaning ""}]]))

(defn handle-term-change [e definition]
  (om/update! definition :term (.. e -target -value)))

(defn handle-meaning-change [e definition]
  (om/update! definition :meaning (.. e -target -value)))

(defn definition-view [definition owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (dom/input #js {:type "text" :placeholder "Term"
                        :value (:term definition)
                        :onChange #(handle-term-change % definition)})
        (dom/div #js {:className "definition"}
          (apply dom/pre #js {:className "bg"}
                 (colorize (:meaning definition)))
          (dom/textarea #js {:className "edit"
                             :value (:meaning definition)
                             :onChange #(handle-meaning-change % definition)}))))))

(defn add-definition [level]
  (om/transact! level #(conj % {:term ""
                                :meaning ""})))

(defn level-view [level owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (apply dom/div nil
          (om/build-all definition-view level))
        (dom/button #js {:onClick #(add-definition level)} "+def")))))

(defn find-terms [levels]
  (apply merge
         (map-indexed (fn [idx level]
                        (apply merge
                               (map #(hash-map (:term %)
                                               idx)
                                    (remove #(empty? (:term %)) level))))
                      levels)))

(defn add-level [app]
  (om/transact! app #(conj % [])))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (apply dom/div nil
          (om/build-all level-view app))
        (dom/button #js {:onClick #(add-level app)} "+level")))))

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "app"))})
