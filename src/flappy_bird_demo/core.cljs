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
(def pillar-gap 200) ;; 158
(def pillar-width 86)
(def pillar-offset-x 900)

(def flap-ampl 15)
(def flap-period 300)

(def pillars-enabled? false)

(def starting-state { 
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
                      [{:idx 0
                        :gap-multiplier 0.5}]})

(defn reset-state [_ cur-time]
  (-> starting-state
      (assoc
          :start-time cur-time
          :flappy-start-time cur-time
          :timer-running true)))

(defn shift-time [state time-shift]
  (-> state
    (update :start-time + time-shift)
    (update :flappy-start-time + time-shift)))

(defonce flap-state (atom (assoc starting-state
                                 :initialized? false)))

(defonce state-history (atom []))

;(defn curr-pillar-pos [pos-x cur-time {:keys [start-time] }]
  ;(translate pos-x horiz-vel (- cur-time start-time)))

(defn in-pillar? [{:keys [cur-x]}]
  (and (>= (+ flappy-x flappy-width)
           cur-x)
       (< flappy-x (+ cur-x pillar-width))))

(defn in-pillar-gap? [flappy-y {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision-with-pillar? [pillar flappy-y]
  (and (in-pillar? pillar)
       (not (in-pillar-gap? flappy-y pillar))))


(defn collision? [{:keys [pillar-list flappy-y] :as st}]
  (if (or (bottom-collision? st)
          (some #(collision-with-pillar? % flappy-y) pillar-list))
    (assoc st :timer-running false)
    st))

(defn new-pillar [idx]
  {:idx idx
   :gap-multiplier (rand)})
   ;:gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn calc-pillars-gap [pillars]
  (map #(assoc % :gap-top
               (max 0
                    (+ 60
                       (floor (* (:gap-multiplier %)
                                 (- bottom-y 120 pillar-gap))))))
       pillars))

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

(defn update-pillars [{:keys [pillar-list start-time-delta pillar-idx] :as st}]
  (let [pillars-with-pos (calc-pillars-pos start-time-delta pillar-list)
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
    (+ start-y (* flap-ampl (.sin js/Math (/ (:time-delta st) flap-period))))))

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
          :time-delta (- timestamp (:flappy-start-time state))
          :start-time-delta (- timestamp (:start-time state)))
      update-flappy
      update-pillars
      (update :pillar-list (partial calc-pillars-pos (:start-time-delta state)))
      (update :pillar-list calc-pillars-gap)
      collision?
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
      (update :pillar-list (partial calc-pillars-pos (:start-time-delta state)))
      (update :pillar-list calc-pillars-gap)
      pillar-offsets))

(defn px [n] (str n "px"))

(defn pillar [{:keys [idx cur-x upper-height lower-height]}]
  [:div.pillars {:key  idx}
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                       :height (px upper-height)}}]
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
     (reset! state-history [])
     (reset! flap-state (reset-state @flap-state time))
     (time-loop time))))

(declare renderer)

(defn time-travel-on-change [ev]
  (let [value (.. ev -target -value)
        state-history-count (count @state-history)
        frame-instance (floor (* (/ value 100) (- state-history-count 1)))
        state (get @state-history frame-instance)]
    ;(. js/console log "switching to" frame-instance)
    ;(println state)
    (renderer (world (assoc state :show-time-travel? true)))))



(defn main-template [{:keys [score cur-time jump-count
                             timer-running border-pos
                             flappy-y pillar-list start-time-delta
                             show-time-travel?]}]
  (sab/html [:div.board {}
             [:h1.score score]
             (if-not timer-running
               [:a.start-button {:onClick #(start-game)}
                (if (< 1 jump-count) "RESTART" "START")]
               [:span])
             [:div (map pillar pillar-list)]
             [:div.flappy {:style {:top (px flappy-y)}}]
             [:div.scrolling-border {:style { :background-position-x (px border-pos)}}]
             (when show-time-travel?
               [:div#time-travel [:input {:type "range"
                                          :min 0
                                          :max 100
                                          :defaultValue 100
                                          :on-change time-travel-on-change}]])]))

(defn toggle-pause []
  (.requestAnimationFrame
    js/window
    (fn [time]
      (swap! flap-state
             (fn [{:keys [paused?] :as s}]
                      (assoc s :paused? (if paused? nil time)))))))

(defn toggle-time-travel []
  (swap! flap-state
         (fn [{:keys [show-time-travel?] :as s}]
           (assoc s :show-time-travel? (if show-time-travel? nil true)))))

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
    " " (do
          (.preventDefault event)
          (cond (:paused? @flap-state) (toggle-pause)
                (:timer-running @flap-state) (swap! flap-state jump)
                :else (start-game)))


          ;(if (:timer-running @flap-state)
            ;(swap! flap-state jump)
            ;(start-game)))
    "p" (toggle-pause)
    "q" (toggle-time-travel)
    nil))

(when-not (:initialized? @flap-state)
  (println "binding keydown")
  (let [body (.-body js/document)]
    (.addEventListener body "keydown" #(handle-key-down %)))
  (swap! flap-state assoc :initialized? true))

(let [node (.getElementById js/document "board-area")]
  (defn renderer [full-state]
    (.render js/ReactDOM (main-template full-state) node))
  (defn clear-ui []
    (.render js/ReactDOM (sab/html [:div]) node)))

(add-watch flap-state :renderer (fn [_ _ _ n]
                                  (renderer (world n))))

(add-watch flap-state :history (fn [_ _ prev new]
                                 (when (not= prev new)
                                   (swap! state-history conj new))))

(reset! flap-state @flap-state)
