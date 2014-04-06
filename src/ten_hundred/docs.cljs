(ns ten-hundred.docs
  (:require [cljs.core.async :refer [chan]]
            [ajax.core :refer [GET PUT]]))

(defn- strip-transient [app]
  (update-in
   app
   [:levels]
   (fn [levels]
     (mapv (fn [level]
            (mapv #(dissoc % :focus) level))
          levels))))

(defn save! [app]
  (PUT (str "https://ten-hundred-files.s3.amazonaws.com/" (:id app))
       {:params (strip-transient app)}))

(defn- readd-transient [app]
  (update-in
   app
   [:levels]
   (fn [levels]
     (mapv (fn [level]
            (mapv #(assoc % :focus (chan)) level))
          levels))))

(defn load [id handler error-handler]
  (GET (str "https://ten-hundred-files.s3.amazonaws.com/" id)
       {:handler (comp handler readd-transient)
        :error-handler error-handler}))
