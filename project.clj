(defproject flappy-bird-demo "0.1.1-SNAPSHOT"
  :description "The original figwheel flappybird demo"

  :url "http://rigsomelight.com/2014/05/01/interactive-programming-flappy-bird-clojurescript.html"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/core.async "0.2.395"]
                 [cljsjs/react-dom-server "15.3.1-0"]  ;; for sablono
                 [cljsjs/react-dom "15.3.1-0"] ;; for sablono
                 [cljsjs/react "15.3.1-0"] ;; for sablono
                 [sablono "0.7.5"]]

  :plugins [[lein-cljsbuild "1.1.4"
             :exclusions [org.clojure/clojure]]
            [lein-figwheel "0.5.8"]
            [cider/cider-nrepl "0.14.0"]]

  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/flappy_bird_demo.js"
                                    :target-path]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "flappy-bird-demo"
              :source-paths ["src"]
              :figwheel true
              :compiler {
                         :main flappy-bird-demo.core
                         :asset-path "js/out"
                         :output-to "resources/public/js/flappy_bird_demo.js"
                         :output-dir "resources/public/js/out"
                         :source-map-timestamp true}}]}

  :figwheel { :css-dirs ["resources/public/css"]}
  :profiles {:dev {:dependencies [[binaryage/devtools "0.8.2"]
                                  [figwheel-sidecar "0.5.8"]
                                  [com.cemerick/piggieback "0.2.1"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                   :repl-options {; for nREPL dev you really need to limit output
                                  :init (set! *print-length* 50)
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})
