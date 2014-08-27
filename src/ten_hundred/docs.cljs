(ns ten-hundred.docs
  (:require [cljs.core.async :refer [chan]]
            [clojure.string :as string]))

(def AppState)

(defn open-disk! []
  )

(defn definition->text [{:keys [term meaning]}]
  (str ":: " term "\n"
       meaning))

(defn level->text [idx level]
  (str ": Level " idx "\n"
       (string/join "\n\n" (map definition->text level))))

(defn levels->text [levels]
  (string/join "\n\n" (map-indexed level->text levels)))

(defn text->levels [text]
  )

(defn text->level [text])

(defn text->definition [text])

(defn save-disk! [levels]
  (let [levels-text (levels->text levels)
        blob (js/Blob. #js [levels-text]
                       #js {:type "text/plain;charset=utf-8"})

        top-definition (first (last (remove empty? levels)))
        title (if (string/blank? (:term top-definition))
                "Untitled"
                (:term top-definition))]
    (js/saveAs blob (str title ".10h"))))

(defn id [app-state]
  (.-id app-state))

(defn save! [app-state levels handler error-handler]
  (let [app-state (or app-state
                      (AppState.))]
    (.set app-state "levels" (clj->js levels))
    (.save app-state nil
           #js {:success
                (fn [as]
                  (js/console.log "save success" as)
                  (handler as))

                :error
                (fn [as error]
                  (js/console.log "save error" as error)
                  (error-handler
                   (str "Save didn't work! Error code " (.-code error)
                        " trying to save document with id " (id as) ".\n"
                        (.-message error) "\n\n"

                        "You should report this bug.")))})))

(defn publish! [app-state levels handler error-handler]
  (if app-state
    (js/Parse.Cloud.run
     "publish" #js {:saveId (id app-state)
                    :levels (clj->js levels)}
     #js {:success
          (fn [response]
            (let [publish-id (.-publishId response)
                  base-location (aget (.match js/window.location.href "(^[^#]*)") 1)]
              (handler (str base-location "#/view/" publish-id))))

          :error
          (fn [error]
            (error-handler
             (str "Publish didn't work! Error code " (.-code error) ":\n"
                  (.-message error))))})

    (error-handler "You need to save your document before trying to publish it.")))

(defn load-publish [publish-id handler error-handler]
  (js/Parse.Cloud.run
   "getPublish" #js {:publishId publish-id}
   #js {:success
        (fn [result]
          (js/console.log "success loading publish " result)
          (handler (js->clj (.-levels result)
                            :keywordize-keys true)))

        :error
        (fn [object error]
          (js/console.log "error loading publish " id object error)
          (error-handler
           (str "Error code " (.-code error) " trying to load document with publish-id " id ".\n"
                (.-message error) "\n\n"

                "Starting a new document.")))}))

(defn load [id handler error-handler]
  (let [query (js/Parse.Query. AppState)]
    (.get query id
          #js {:success
               (fn [app-state]
                 (js/console.log "success loading" app-state)
                 (handler app-state
                          (js->clj (.get app-state "levels")
                                   :keywordize-keys true)))

               :error
               (fn [object error]
                 (js/console.log "error loading" id object error)
                 (error-handler
                  (str "Error code " (.-code error) " trying to load document with id " id ".\n"
                       (.-message error) "\n\n"

                       "Starting a new document.")))})))

(defn init! []
  (when (aget js/window "Parse")
    (js/Parse.initialize "WlNgee8GBcq0tDvv2x6jQdWlWtPMNbZcNdvI3amd"
                         "y2zxRukimVk6uXOiqzWrGui2A1ca9ugAgLvO1Wxr")
    (set! AppState (js/Parse.Object.extend "AppState"))))
