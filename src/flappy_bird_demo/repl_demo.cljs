(ns flappy-bird-demo.repl-demo
  (:require ))

(comment
  (def an-animal (.createElement
                   js/document "div"))

  (aset an-animal "className" "flappy")


  (.appendChild (.-body js/document) an-animal)

  ;;:Piggieback (figwheel-sidecar.repl-api/repl-env)

  )
