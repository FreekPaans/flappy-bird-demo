(ns flappy-bird-demo.repl-demo
  (:require ))

(comment
  (def body (aget js/document "body"))

  (def an-animal (.createElement js/document "div"))

  (aset an-animal "className" "flappy")


  (.appendChild body an-animal)

  )
