(ns ten-hundred.terms
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]

            [cljs.core.async :refer [put!]]
            [clojure.string :as string]
            [goog.dom.classlist :as classlist]

            [ten-hundred.dict :as dict]
            [ten-hundred.tex :as tex]
            [ten-hundred.hover :as hover]))

(defn escape-term [term]
  (string/replace term #"[^A-Za-z0-9_']" "_"))

(defn get-synonyms [input]
  (-> input
      (string/replace #"[^A-Za-z0-9_' ]" "_")
      (string/split #" ")
      (#(remove string/blank? %))))

(defn top-definition-path [levels]
  (->> levels
       (map-indexed
        (fn [level-idx level]
          (map-indexed
           (fn [definition-idx definition]
             [level-idx definition-idx])
           level)))
       (apply concat)
       last))

(defn render-tex [tex]
  (om/build tex/tex {:style {}
                     :text tex}))

(defn find-terms [levels]
  (apply concat
    (map-indexed
     (fn [level-idx level]
       (->> level
            (map-indexed (fn [definition-idx {:keys [term synonyms meaning]}]
                           (hash-map :term (string/lower-case term)
                                     :synonyms (map string/lower-case synonyms)
                                     :meaning meaning
                                     :path [level-idx definition-idx])))
            (remove #(and (string/blank? (:term %))
                          (empty? (:synonyms %))))))
     levels)))

(defn find-term [terms token]
  (last (filter #(or (= (:term %) token)
                     (some #{token} (:synonyms %)))
                terms)))

(defn colorize-word [self-term highlight terms word idx]
  (let [lc-word (string/lower-case word)
        term-state (when terms (find-term terms lc-word))]
    (cond term-state
          (let [[level-idx definition-idx] (:path term-state)]
            (om/build hover/span-with-hover-view
                      {:content
                       (dom/span {:class "defined"

                                  :data-level-idx level-idx
                                  :data-definition-idx definition-idx

                                  :data-idx idx}
                         word)

                       :hover-style
                       {:width (min 400 (+ 20 (* 7 (count (:meaning term-state)))))}

                       :hover-content
                       #(word-map {:word identity
                                   :image identity
                                   :tex render-tex}
                                  (:meaning term-state))}))

          (= (string/lower-case self-term) lc-word)
          (dom/span {:class "self"} word)

          (dict/words lc-word)
          word

          highlight
          (dom/span {:class "notDefined"} word)

          :else
          word)))

(defn word-map [{:keys [word image tex]} text]
  (->> text
       (js/THParser.parse)
       (map-indexed (fn [idx token]
                      (let [kind (.-kind token)
                            data (.-data token)]
                        (case kind
                          "spacing" data
                          "tex" (tex data idx)
                          "image" (image (.-alt data) (.-src data) idx)
                          "word" (word data idx)))))))

(defn word-click! [control target]
  (cond (classlist/contains target "notDefined") ; not defined term
        (put! control [:define (.-innerText target)])

        (classlist/contains target "defined")
        (let [level-idx (js/parseInt (.-levelIdx (.-dataset target)))
              definition-idx (js/parseInt (.-definitionIdx (.-dataset target)))]
          (put! control [:author [level-idx definition-idx]]))))

