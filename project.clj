(defproject ten-hundred "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.2.395"]
                 [om "0.7.1"]
                 [prismatic/om-tools "0.3.2"]
                 [org.clojure/core.match "0.3.0-alpha4"]]

  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-haml-sass "0.2.7-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "ten-hundred"
              :source-paths ["src"]
              :compiler {
                :output-to "ten_hundred.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]}

  :scss {:src "scss/"
         :output-directory "out/"
         :output-extension "css"})
