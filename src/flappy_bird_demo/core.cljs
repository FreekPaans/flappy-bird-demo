(ns flappy-bird-demo.core
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

(def horiz-vel -0.15)
(def gravity 0.05)
(def jump-vel 21)
(def start-y 312)
(def bottom-y 561)
(def flappy-x 212)
(def flappy-width 57)
(def flappy-height 41)
(def pillar-spacing 324)
(def pillar-gap 158) ;; 158
(def pillar-width 86)
(def pillar-offset-x 900)

(def starting-state { :run-game? true
                     :initialized? true
                      :timer-running false
                      :jump-count 0
                      :initial-vel 0
                      :start-time 0
                      :flappy-start-time 0
                      :flappy-y   start-y
                      :pillar-offset-x 900
                      :pillar-idx 1
                      :pillar-list
                      [{ :start-time 0
                         :pos-x 900
                         :cur-x 900
                         :gap-top 200 }]})

(defn reset-state [_ cur-time]
  (-> starting-state
      (update-in [:pillar-list] (fn [pls] (map #(assoc % :start-time cur-time) pls)))
      (assoc
          :start-time cur-time
          :flappy-start-time cur-time
          :timer-running true)))

(defn shift-time [state time-shift]
  (-> state
    (update-in [:pillar-list] (fn [pls]
                                (map #(update % :start-time + time-shift) pls)))
    (update :start-time + time-shift)
    (update :flappy-start-time + time-shift)))

(defonce flap-state (atom (assoc starting-state ;:run-game? false
                                 :initialized? false)))

;(defn curr-pillar-pos [pos-x cur-time {:keys [start-time] }]
  ;(translate pos-x horiz-vel (- cur-time start-time)))

(defn in-pillar? [{:keys [cur-x]}]
  (and (>= (+ flappy-x flappy-width)
           cur-x)
       (< flappy-x (+ cur-x pillar-width))))

(defn in-pillar-gap? [{:keys [flappy-y]} {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision? [{:keys [pillar-list] :as st}]
  (if (some #(or (and (in-pillar? %)
                      (not (in-pillar-gap? st %)))
                 (bottom-collision? st)) pillar-list)
    (assoc st :timer-running false)
    st))

(defn new-pillar [idx]
  {:idx idx
   :gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn calc-pillars-pos [time-delta pillars]
  (map (fn [{:keys [idx] :as  pl}]
         (assoc pl :cur-x
                (+ pillar-offset-x
                   (* pillar-spacing idx)
                   (* horiz-vel time-delta)))) pillars))
                ;(curr-pillar-pos
                  ;(+ pillar-offset-x
                     ;(* pillar-spacing
                        ;idx))
                  ;cur-time pl))) pillars))

(defn update-pillars [{:keys [pillar-list time-delta pillar-idx] :as st}]
  (let [pillars-with-pos (calc-pillars-pos time-delta pillar-list)
        pillars-in-world (sort-by
                          :cur-x
                          (filter #(> (:cur-x %) (- pillar-width)) pillars-with-pos))
        new-pillar? (< (count pillars-in-world) 3)]
    (assoc
      st
      :pillar-list
      (if new-pillar?
        (conj pillars-in-world (new-pillar pillar-idx))
        pillars-in-world)
      :pillar-idx (if new-pillar? (inc pillar-idx) pillar-idx))))

(defn sine-wave [st]
  (assoc st
    :flappy-y
    (+ start-y (* 30 (.sin js/Math (/ (:time-delta st) 300))))))

(defn update-flappy [{:keys [time-delta initial-vel flappy-y jump-count] :as st}]
  (if (pos? jump-count)
    (let [cur-vel (- initial-vel (* time-delta gravity))
          new-y   (- flappy-y cur-vel)
          new-y   (if (> new-y (- bottom-y flappy-height))
                    (- bottom-y flappy-height)
                    new-y)]
      (assoc st
        :flappy-y new-y))
    (sine-wave st)))

(defn score [{:keys [cur-time start-time] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- cur-time start-time) horiz-vel) 544)
                               pillar-spacing)))
                 4)]
  (assoc st :score (if (neg? score) 0 score))))

(defn time-update [timestamp state]
  (-> state
      (assoc
          :cur-time timestamp
          :time-delta (- timestamp (:flappy-start-time state)))
      update-flappy
      update-pillars
      (update-in [:pillar-list] #(calc-pillars-pos (:time-delta state) %))
      ;collision?
      score))

(defn jump [{:keys [cur-time jump-count] :as state}]
  (-> state
      (assoc
          :jump-count (inc jump-count)
          :flappy-start-time cur-time
          :initial-vel jump-vel)))

;; derivatives

(defn border [{:keys [cur-time] :as state}]
  (-> state
      (assoc :border-pos (mod (translate 0 horiz-vel cur-time) 23))))

(defn pillar-offset [{:keys [gap-top] :as p}]
  (assoc p
    :upper-height gap-top
    :lower-height (- bottom-y gap-top pillar-gap)))

(defn pillar-offsets [state]
  (update-in state [:pillar-list]
             (fn [pillar-list]
               (map pillar-offset pillar-list))))

(defn world [state]
  (-> state
      border
      pillar-offsets))

(defn px [n] (str n "px"))

(defn pillar [{:keys [idx cur-x upper-height lower-height]}]
  [:div.pillars {:key  idx}
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                       :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left (px cur-x)
                                       :height lower-height}}]])

(defn time-loop [time]
  (when-not (:paused? @flap-state)
    (let [new-state (swap! flap-state (partial time-update time))]
      (when (:timer-running new-state)
        (go
          (<! (timeout 30))
          (.requestAnimationFrame js/window time-loop))))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (reset! flap-state (reset-state @flap-state time))
     (time-loop time))))

(defn main-template [{:keys [score cur-time jump-count
                             timer-running border-pos
                             flappy-y pillar-list]}]
  (sab/html [:div.board { :onMouseDown (fn [e]
                                         (swap! flap-state jump)
                                         (.preventDefault e)) }
             [:h1.score score]
             (if-not timer-running
               [:a.start-button {:onClick #(start-game)}
                (if (< 1 jump-count) "RESTART" "START")]
               [:span])
             [:div (map pillar pillar-list)]
             [:div.flappy {:style {:top (px flappy-y)}}]
             [:div.scrolling-border {:style { :background-position-x (px border-pos)}}]]))

(defn toggle-pause []
  (println "pausing")
  (.requestAnimationFrame
    js/window
    (fn [time]
      (swap! flap-state
             (fn [{:keys [paused?] :as s}]
                      (assoc s :paused? (if paused? nil time)))))))

(add-watch flap-state :pause-handle
           (fn [_ _ {paused-old? :paused?} {paused-new? :paused?}]
             (when (and paused-old? (not paused-new?))
               (.requestAnimationFrame
                 js/window
                 (fn [time]
                   (let[pause-delta (- time paused-old?)]
                     (swap! flap-state shift-time pause-delta)
                     (time-loop time)))))))

(defn handle-key-down [event]
  (case (.-key event)
        "p" (toggle-pause)
        nil))

(when-not (:initialized? @flap-state)
  (println "binding keydown")
  (let [body (.-body js/document)]
    (.addEventListener body "keydown" #(handle-key-down %)))
  (swap! flap-state assoc :initialized? true))

(defn run-game! []
  (swap! flap-state assoc :run-game? true))

(defn stop-game! []
  (swap! flap-state assoc :run-game? false))

(let [node (.getElementById js/document "board-area")]
  (defn renderer [full-state]
    (.render js/ReactDOM (main-template full-state) node))
  (defn clear-ui []
    (.render js/ReactDOM (sab/html [:div]) node)))

(add-watch flap-state :renderer (fn [_ _ _ n]
                                  (if (:run-game? n)
                                    (renderer (world n))
                                    (clear-ui))))

(reset! flap-state @flap-state)
