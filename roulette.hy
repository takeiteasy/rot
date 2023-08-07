#!/usr/bin/env hy
(import threading [Timer Thread])
(import asyncio)
(import queue [Queue])
(import time)
(import math)
(import re)
(import enum [Enum])
(import random [randint])

(import pyray *)
(import transitions [Machine])
(import redis)
(import twitchAPI [Twitch])
(import twitchAPI.oauth [UserAuthenticator])
(import twitchAPI.types [AuthScope ChatEvent])
(import twitchAPI.chat [Chat EventData ChatMessage ChatSub ChatCommand])

(defmacro percent [a b]
  `(* (/ ~a ~b) 100))

(defmacro neg [n]
  `(- 0 ~n))

(defmacro zero? [x]
  `(= ~x 0))

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(defmacro unless [expr #* body]
  `(when (not ~expr)
     (do
       ~@body)))

(defmacro uniq [a]
  `(list (set ~a)))

(defmacro clamp [n mi ma]
  `(max (min ~n ~ma) ~mi))

(defmacro remap [v low1 high1 low2 high2]
  `(+ ~low2 (* (- ~v ~low1) (/ (- ~high2 ~low2) (- ~high1 ~low1)))))

(defmacro if-match [string pattern pos-body neg-body]
  `(let [$ (re.match ~pattern ~string)]
     (if $
       ~pos-body
       ~neg-body)))

(defclass InvalidBet [Exception]
  (defn __init__ [self [reason None]]
    (setv self.reason reason)
    (Exception.__init__ self))

  (defn bet-format [self]
    "!bet command format: `!bet [amount] [positions]`")

  (defn __str__ [self]
    (if self.reason
      (.join " -- " [f"Sorry, I can't understand \"{self.reason}\"" (self.bet-format)])
      (self.bet-format))))

(defclass InvalidUser [Exception]
  (defn __str__ [self]
    "You are not registered, type `!register` to start!"))

(defclass InsufficientFundsError [Exception]
  (defn __init__ [self amount total]
    (setv
      self.amount amount
      self.total total)
    (Exception.__init__ self))

  (defn __str__ [self]
    f"Cannot place bet for `${self.amount}` you only have `${total}` available"))

(defclass Task []
  (defn __init__ [self interval callback [start False] [repeating False]]
    (setv
      self.timer None
      self.interval interval
      self.callback callback
      self.running False
      self.repeating repeating
      self.start-time None)
    (when start
      (self.start)))

  (defn run [self]
    (setv self.running False)
    (self.callback)
    (when self.repeating
      (self.start)))

  (defn start [self]
    (when (not self.running)
      (setv
        self.timer (Timer self.interval self.run)
        self.start-time (time.time)
        self.running True)
      (.start self.timer)))

  (defn elapsed-time [self]
    (if self.running
      (- (time.time) self.start-time)
      0))
  
  (defn remaining-time [self]
    (if self.running
      (- self.interval (self.elapsed-time))
      0))

  (defn stop [self]
    (.cancel self.timer)
    (setv self.running False)))

(defclass Animation []
  (defn __init__ [self callback timeout]
    (setv
      self.timeout timeout
      self.task (Task timeout self.end)
      self.callback callback
      self.running False
      self.thread None))
  
  (defn start-thread [self]
    (let [t (Thread :target self.loop)]
      (setv self.thread t)
      (.start self.thread)))
  
  (defn loop [self]
    (while self.running
      (self.callback (.elapsed-time self.task) self.timeout)
      (time.sleep 0.1)))
  
  (defn begin [self]
    (if self.running
      (do
        (.stop self.task)
        (setv self.running False)
        (.begin self))
      (do
        (setv self.running True)
        (.start self.task)
        (.start-thread self))))
  
  (defn end [self]
    (when self.running
      (setv self.running False)
      (.stop self.task))))

(defmacro with-alpha [n c]
  `(if (= self.current-number ~n)
     ~c
     (color-alpha ~c self.alpha)))

(defclass Wheel []
  (setv states ["stopped" "spinning" "slowing"])

  (defn __init__ [self]
    (setv
      self.machine (Machine :model self :states Wheel.states :initial "stopped")
      self.speed 0.0
      self.camera (Camera2D)
      self.camera.target (Vector2 **half-wheel-size**.x 0)
      self.camera.offset (Vector2 (/ **window-size**.x 2) 0)
      self.camera.rotation 0.0
      self.camera.zoom 1.0
      self.current-number 0
      self.alpha 0.5)
    (.add-transition self.machine :trigger "stop" :source "*" :dest "stopped" :before "reset_speed")
    (.add-transition self.machine :trigger "reset" :source "*" :dest "stopped" :before "reset_speed_position")
    (.add-transition self.machine :trigger "start" :source "stopped" :dest "spinning")
    (.add-transition self.machine :trigger "slow" :source "spinning" :dest "slowing"))
  
  (defn reset-speed [self]
    (setv self.speed 0.0))
  
  (defn reset-speed-position [self]
    (setv
      self.speed 0.0
      self.camera.target (Vector2 **half-wheel-size**.x 0)))
  
  (defn draw-segment [self i x]
    (let [w (int **wheel-size**.x)
          h (int **wheel-size**.y)
          n (get **wheel-numbers** i)
          text (str n)]
      (draw-rectangle (int x) 0 w h (with-alpha n (get **wheel-colors** i)))
      (draw-rectangle-lines (int x) 0 w h (with-alpha n GRAY))
      (draw-text text (int (- (+ x **half-wheel-size**.x) (/ (measure-text text **font-size**) 2))) (int (- **half-wheel-size**.y 15)) **font-size** (with-alpha n WHITE))))

  (defn draw [self]
    (when (= self.state "slowing")
      (setv self.speed (- self.speed 0.1))
      (when (<= self.speed 1)
        (self.stop)))
    (setv self.camera.target.x (+ self.camera.target.x self.speed))
    (when (> self.camera.target.x **max-wheel-size**)
      (setv self.camera.target.x (abs (- **max-wheel-size** self.camera.target.x))))
    (setv self.current-number (get **wheel-numbers** (math.floor (/ self.camera.target.x **wheel-size**.x))))
    (begin-mode-2d self.camera)
    (for [i (range 0 (len **wheel-colors**))]
      (self.draw-segment i (* i **wheel-size**.x)))
    (let [half-width (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x)]
      (cond
        (< self.camera.target.x half-width) (for [i (range (- (len **wheel-colors**) 1) (- (len **wheel-colors**) **max-visible-numbers**) -1)]
                                              (self.draw-segment i (neg (* (- (len **wheel-colors**) i) **wheel-size**.x))))
        (> self.camera.target.x (- **max-wheel-size** half-width)) (for [i (range 0 **max-visible-numbers**)]
                                                                     (self.draw-segment i (+ **max-wheel-size** (* i **wheel-size**.x))))))
    (draw-line-ex (Vector2 self.camera.target.x 0) (Vector2 self.camera.target.x **wheel-size**.y) 2.0 (with-alpha -1 WHITE))
    (end-mode-2d)))

(defmacro draw-numbered-box [x y sz n]
  `(self.draw-text-box ~x ~y ~sz (str ~n) (get **wheel-colors** ~n)))

(defclass Table []
  (defn __init__ [self]
    (setv
      self.camera (Camera2D)
      self.camera.target (Vector2 0 0)
      self.camera.offset (Vector2 0 **wheel-size**.y)
      self.camera.rotation 0.0
      self.camera.zoom 1.0
      self.alpha 1.0))

  (defn draw-box [self x y sz col [border-col WHITE]]
    (draw-rectangle (int x) (int y) (int sz.x) (int sz.y) col)
    (draw-rectangle-lines (int x) (int y) (int sz.x) (int sz.y) border-col))
  
  (defn draw-text-box [self x y sz text col [border-col WHITE] [text-col WHITE]]
    (let [text-width (/ (measure-text text **font-size**) 2)]
      (self.draw-box x y sz col border-col)
      (draw-text text (int (+ x (- (/ sz.x 2) text-width))) (int (+ y (- (/ sz.y 2) 15))) **font-size** text-col)))
  
  (defn draw [self]
    (begin-mode-2d self.camera)
    (let [ox (int (- (/ **window-size**.x 2) (/ (* **table-box-size**.x 12) 2)))
          oy (int (* **table-box-size**.y 3))]
      (draw-numbered-box (- ox **table-box-size**.x) **table-box-size**.y (Vector2 (int **table-box-size**.x) oy) 0)
      (for [[i text] (enumerate ["1st 12" "2nd 12" "3rd 12"])]
        (let [w (* **table-box-size**.x 4)
              fw (* **table-box-size**.x 12)]
          (self.draw-text-box (+ ox fw) (+ **table-box-size**.y (* **table-box-size**.y i)) **table-box-size** "2 to 1" GREEN)
          (self.draw-text-box (+ ox (* w i)) (+ oy **table-box-size**.y) (Vector2 w (/ **table-box-size**.y 2)) text GREEN)))
      (let [w (* **table-box-size**.x 2)
            h (/ **table-box-size**.y 2)
            sz (Vector2 w h)]
        (for [i (range 0 6)]
          (let [x (+ ox (* w i))
                y (+ oy **table-box-size**.y h)]
            (match i
              0 (self.draw-text-box x y sz "1 to 18" DARKGREEN)
              1 (self.draw-text-box x y sz "Even" DARKGREEN)
              2 (self.draw-box x y sz RED)
              3 (self.draw-box x y sz BLACK)
              4 (self.draw-text-box x y sz "Odd" DARKGREEN)
              5 (self.draw-text-box x y sz "19 to 36" DARKGREEN))))) 
      (for [x (range 1 4)]
        (for [[i y] (enumerate (range x (+ x 34) 3))]
          (let [fx (int (+ ox (* **table-box-size**.x i)))]
            (draw-numbered-box fx oy **table-box-size** y)))
        (setv oy (- oy (int **table-box-size**.y)))))
    (end-mode-2d)))

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

(defn/a on-ready [event]
  (print "Twitch client is ready!")
  (await (event.chat.join-room **host-channel**)))

(defmacro user-exist? [uid [table "leaderboard"]]
  `(not (= (.zscore **db** ~table ~uid) None)))

(defmacro if-user-exist? [uid pos-body neg-body]
  `(do
     (if (user-exist? ~uid)
       ~pos-body
       ~neg-body)))

(defn user-main-balance [uid]
  (if-user-exist? uid
                  (int (.zscore **db** "leaderboard" uid))
                  (raise InvalidUser)))

(defn user-temporary-balance [uid]
  (if (user-exist? uid "leaderboard:tmp")
    (.zscore **db** "leaderboard:tmp" uid)
    (let [balance (user-main-balance uid)]
      (do
        (.zadd **db** "leaderboard:tmp" {uid balance})
        balance))))

(defn user-balance [uid]
  (if (= **coupier**.state "betting")
    (user-temporary-balance uid)
    (user-main-balance uid)))

(defn/a on-register [cmd]
  (if-user-exist? cmd.user.id
                  (await (cmd.reply f"You're already registered..."))
                  (do
                    (.zadd **db** "leaderboard" {cmd.user.id 1000})
                    (await (cmd.reply f"You're now registered! You have a balance of $1000")))))

(defmacro set-first-part [n]
  `(do
     (setv first-part ~n)
     (self.OnFirstPart)))

(defmacro valid-number? [n]
  `(in ~n **wheel-numbers**))

(defmacro fast-valid-number? [n]
  `(and (> ~n 0)
        (< (len **wheel-numbers**))))

(defmacro test-first-part [#* parts]
  `(let [n (match first-part ~@parts)]
    (if (not n)
      (raise (InvalidBet))
      (do
        (concat-numbers (list n))
        (self.OnSecondPart)))))

(defmacro concat-numbers [body]
  `(setv numbers (+ numbers ~body)))

(defclass BetParser []
  (setv states ["ExpectAmount" "ExpectNumbers" "ExpectSecondPart"])

  (defn __init__ [self string]
    (setv
      self.input string 
      self.machine (Machine :model self :states BetParser.states :initial "ExpectAmount"))
    (.add-transition self.machine :trigger "OnAmount" :source "ExpectAmount" :dest "ExpectNumbers")
    (.add-transition self.machine :trigger "OnFirstPart" :source "ExpectNumbers" :dest "ExpectSecondPart")
    (.add-transition self.machine :trigger "OnSecondPart" :source "ExpectSecondPart" :dest "ExpectNumbers")
    (.add-transition self.machine :trigger "FinishedBet" :source "ExpectNumbers" :dest "ExpectAmount"))

  (defn parse [self]
    (for [tokens (lfor x (.split self.input ",")
                         (lfor y (.split x " ")
                               :if y
                               (.lower y)))]
        (let [amount 0 numbers [] first-part None]
          (for [token tokens]
            (match self.state
              "ExpectAmount" (if-match token r"^\$?(\d+)$"
                                       (do
                                         (setv amount (int (get $ 1)))
                                         (self.OnAmount))
                                       (raise InvalidBet))
              "ExpectNumbers" (cond
                                (re.match r"^\d+$" token) (let [t (int token)]
                                                            (if (fast-valid-number? t)
                                                              (.append numbers t)
                                                              (raise (InvalidBet))))
                                (re.match r"^(-?\d){2,4}$" token) (concat-numbers (lfor n (.split token "-")
                                                                                        :setv i (int n)
                                                                                        :if (fast-valid-number? i)
                                                                                        i))
                                (= token "red") (concat-numbers **red**)
                                (= token "black") (concat-numbers **black**)
                                (= token "even") (concat-numbers (list (range 2 37 2)))
                                (= token "odd") (concat-numbers (list (range 1 37 2)))
                                (or (= token "first")
                                    (= token "1st")) (set-first-part 1)
                                (or (= token "second")
                                    (= token "2st")) (set-first-part 2)
                                (or (= token "third")
                                    (= token "3rd")) (set-first-part 3)
                                True (unless (in token ["and" "or" "on"])
                                             (raise (InvalidBet))))
              "ExpectSecondPart" (cond
                                   (= token "half") (test-first-part
                                                     1 (range 1 19)
                                                     2 (range 19 37))
                                   (re.match r"^col(umn)?$" token) (test-first-part
                                                                    1 (range 1 37 3)
                                                                    2 (range 2 37 3)
                                                                    3 (range 3 37 3))
                                   (re.match r"^(twelve|12|dozen)$" token) (test-first-part
                                                                            1 (range 1  13)
                                                                            2 (range 13 25)
                                                                            3 (range 25 37))
                                   True (raise (InvalidBet)))))
          (self.FinishedBet)
          (yield #(amount (uniq numbers)))))))

(defclass Bet []
  (defn __init__ [self user amount numbers]
    (setv
      self.user user
      self.amount amount 
      self.numbers numbers)) 

  (defn validate [self]
    (cond
      (not (user-exist? self.user)) (raise (InvalidUser))
      (or (zero? self.amount)
          (zero? (len self.numbers))) (raise (InvalidBet))
      True (let [balance (user-balance self.user)]
             (if (not balance)
               (raise InvalidUser)
               (if (> balance self.amount)
                 True
                 (raise (InsufficientFundsError self.amount balance)))))))
  
  (defn __str__ [self]
    f"(Bet uid:{self.user} amount:{self.amount} numbers:{self.numbers})"))  

(defn resolve-bets [winner]
  (while (not (.empty **bets**))
    (let [bet (.get **bets**)]
      (when (in winner bet.numbers)
        (.zadd **db** "leaderboard:tmp" {bet.user (user-temporary-balance bet.user)}))))
  (for [balance (.zrange **db** "leaderboard:tmp" 0 -1 True)]
    (.zadd **db** "leaderboard" {bet.user (user-temporary-balance bet.user)}))
  (.delete **db** "leaderboard:tmp"))

(defn/a on-bet [cmd]
  (when (> (len cmd.parameter) 0)
    (try
      (let [bets (lfor [amount numbers] (.parse (BetParser cmd.parameter)) (Bet cmd.user.id amount numbers))
            total (sum (lfor bet bets bet.amount))
            balance (- (user-balance cmd.user.id) total)]
        (try
          (cond
            (< balance 0) (raise (InsufficientFundsError total balance))
            (or (zero? (len bets))
                (zero? total)) (raise (InvalidBet)))
          (except [e [InvalidBet InsufficientFundsError]]
                  (await (cmd.reply (str e)))))
        (for [bet bets]
          (try
            (.validate bet)
            (.put **bets** bet)
            (except [e [InvalidBet InsufficientFundsError]]
                    (await (cmd.reply (str e))))))
        (.zadd **db** "leaderboard:tmp" {cmd.user.id balance})
        (await (cmd.reply f"Bet{(if (> (len bets) 1) "s" "")} placed for ${total}! You have ${balance} remaining")))
      (except [e [InvalidUser]]
              (await (cmd.reply (str e)))))))

(defn/a on-balance [cmd]
  (try
    (let [balance (user-main-balance cmd.user.id)
          tmp-balance (user-temporary-balance cmd.user.id)]
      (await (cmd.reply (if (= balance 0)
                          "You're bust!"
                          f"You have ${balance} in your account and ${tmp-balance} available"))))
    (except [e InvalidUser]
            (await (cmd.reply (str e))))))

(defn/a run []
  (let [twitch (await (Twitch **app-id** **app-secret**))
        auth (UserAuthenticator twitch **user-scope**)
        tokens (await (.authenticate auth))]
    (await (.set-user-authentication twitch (get tokens 0) **user-scope** (get tokens 1)))
    (let [chat (await (Chat twitch))]
      (.delete **db** "leaderboard:tmp")
      (.register-event chat ChatEvent.READY on-ready)
      ; (.register-event chat ChatEvent.RAID on-raid)
      (.register-command chat "register" on-register)
      (.register-command chat "bet" on-bet)
      (.register-command chat "balance" on-balance)
      (.start chat) 

      (init-window (int **window-size**.x) (int **window-size**.y) "Twitch Roulette")
      (set-target-fps 60)
      (.start **coupier**)
      ; (toggle-fullscreen)
      (while (not (window-should-close))
        (begin-drawing)
        (clear-background BLACK)
        (.draw **coupier**)
        (end-drawing))
      (.kill **coupier**)
      (.stop chat)
      (await (.close twitch)))))

(defmacro if-color [c]
  `(lfor [x y] (enumerate **wheel-colors**)
         :if (= y ~c)
         x))

(setv
  **app-id** (read-file "twitch-token.txt")
  **app-secret** (read-file "twitch-secret.txt")
  **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT]
  **host-channel** "roryb_bellows"
  **window-size** (Vector2 1920 1080)
  **wheel-colors** [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED RED BLACK RED BLACK RED BLACK RED BLACK RED]
  **wheel-numbers** [0 32 15 19 4 21 2 25 17 34 6 27 13 36 11 30 8 23 10 5 24 16 33 1 20 14 31 9 22 18 29 7 28 12 35 3 26]
  **red** (if-color RED)
  **black** (if-color BLACK)
  **wheel-size** (Vector2 300 300)
  **half-wheel-size** (Vector2 (/ **wheel-size**.x 2) (/ **wheel-size**.y 2))
  **max-wheel-speed** 100.0
  **max-wheel-size** (* (len **wheel-colors**) **wheel-size**.x)
  **max-visible-numbers** (math.ceil (/ **window-size**.x **wheel-size**.x))
  **visible-numbers-size** (* **max-visible-numbers** **wheel-size**.x)
  **font-size** 32
  **table-box-size** (Vector2 125 125)
  **betting-phase-timeout** 10
  **spinning-phase-min** 5
  **spinning-phase-max** 5
  **end-phase-timeout** 5
  **db** (redis.Redis "localhost" 6379 0)
  **bets** (Queue)
  **coupier** (Croupier))

(.run asyncio (run))