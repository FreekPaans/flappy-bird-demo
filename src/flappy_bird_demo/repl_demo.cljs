(ns flappy-bird-demo.repl-demo
  (:require ))

(comment
  (println "hello utrechtjs")

  (def body (aget js/document "body"))

  (def an-animal (.createElement js/document "div"))

  (aset an-animal "className" "flappy")


  (.appendChild body an-animal)

  ;;:Piggieback (figwheel-sidecar.repl-api/repl-env)

  )
