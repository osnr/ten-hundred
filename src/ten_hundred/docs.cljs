(ns ten-hundred.docs
  (:require [cljs.core.async :refer [chan]]
            [clojure.string :as string]))

(def AppState)

(defn open-disk! []
  )

(defn definition->text [{:keys [term meaning]}]
  (str "## " term "\n"
       meaning))

(defn level->text [idx level]
  (str "# Level " idx "\n"
       (string/join "\n\n" (map definition->text level))))

(defn levels->text [levels]
  (string/join "\n\n" (map-indexed level->text levels)))

(defn text->levels []
  )

(defn save-disk! [levels]
  (js/console.log (levels->text levels)))

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
  ;; (if app-state
  ;;   (if-let [publish-id (.get app-state "publishId")]
  ;;     ;; mhh this is kind of ugly
  ;;     ((js/Parse.Query. AppState) .get publish-id)
  ;;     )
  ;;   (error-handler "You should save before publishing."))
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
  (js/Parse.initialize "WlNgee8GBcq0tDvv2x6jQdWlWtPMNbZcNdvI3amd"
                       "y2zxRukimVk6uXOiqzWrGui2A1ca9ugAgLvO1Wxr")
  (set! AppState (js/Parse.Object.extend "AppState")))
