(defproject ten-hundred "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.3"]
                 [org.clojars.franks42/cljs-uuid-utils "0.1.3"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "ten-hundred"
              :source-paths ["src"]
              :compiler {
                :output-to "ten_hundred.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
