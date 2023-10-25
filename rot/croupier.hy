(import rot.table *)
(import rot.database *)
(import random [randint])

(setv
  **betting-phase-timeout** 10
  **spinning-phase-min** 5
  **spinning-phase-max** 5
  **end-phase-timeout** 5)

(defclass Croupier []
  (setv states ["betting" "spinning" "slowing" "end"])

  (defn __init__ [self]
    (setv
      self.machine (Machine :model self :states Croupier.states :initial "betting")
      self.task (Task **betting-phase-timeout** self.next-state)
      self.wheel (Wheel)
      self.table (Table)
      self.wheel-fade-in (Animation self.fade-wheel-in 1.0)
      self.wheel-fade-out (Animation self.fade-wheel-out 1.0))
    (.add-transition self.machine :trigger "next" :source "betting" :dest "spinning" :before "spin_wheel")
    (.add-transition self.machine :trigger "next" :source "spinning" :dest "slowing" :before "slow_wheel")
    (.add-transition self.machine :trigger "next" :source "slowing" :dest "end" :before "new_game_task")
    (.add-transition self.machine :trigger "next" :source "end" :dest "betting" :before "start_game_task"))

  (defn start [self]
    (.start self.task))

  (defn spin-wheel [self]
    (setv
      self.task.interval (+ **spinning-phase-min** (randint 0 **spinning-phase-max**))
      self.wheel.speed **max-wheel-speed**)
    (resolve-user-stakes)
    (.begin self.wheel-fade-in)
    (.start self.task)
    (.start self.wheel))

  (defn slow-wheel [self]
    (.slow self.wheel))

  (defn new-game-task [self]
    (setv self.task.interval **end-phase-timeout**)
    (.start self.task)
    (.begin self.wheel-fade-out))

  (defn start-game-task [self]
    (setv self.task.interval **betting-phase-timeout**)
    (.start self.task))

  (defn next-state [self]
    (self.next))

  (defn fade-wheel-in [self elapsed-time target-time]
    (setv self.wheel.alpha (remap (clamp (/ elapsed-time target-time) 0.0 1.0) 0.0 1.0 0.5 1.0)))

  (defn fade-wheel-out [self elapsed-time target-time]
    (setv self.wheel.alpha (remap (clamp (/ elapsed-time target-time) 0.0 1.0) 0.0 1.0 1.0 0.5)))

  (defn draw-betting [self]
    (draw-text f"Betting! {(math.ceil (.remaining-time self.task))}" 5 5 20 LIME))

  (defn draw-spin [self]
    (when (= self.wheel.state "stopped")
      (resolve-user-bets self.wheel.current-number)
      (self.next))
    (draw-text "Spinning!" 5 5 20 ORANGE))

  (defn draw-end [self]
    (draw-text f"Waiting! {(math.ceil (.remaining-time self.task))}" 5 5 20 RED))

  (defn draw [self]
    (.draw self.table)
    (.draw self.wheel)
    ((get [self.draw-betting self.draw-spin self.draw-spin self.draw-end] (.index self.states self.state))))

  (defn kill [self]
    (try
      (.stop self.task)
      (except [e Exception]
              `()))))