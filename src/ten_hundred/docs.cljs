(ns ten-hundred.docs
  (:require [cljs.core.async :refer [chan]]
            [ajax.core :refer [GET PUT]]))

(defn save! [id app]
  (PUT (str "https://ten-hundred-files.s3.amazonaws.com/" id)
       {:params app}))

(defn load [id handler error-handler]
  (GET (str "https://ten-hundred-files.s3.amazonaws.com/" id)
       {:handler handler
        :error-handler error-handler}))
