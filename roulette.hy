#!/usr/bin/env hy
(import threading [Timer Thread])
(import asyncio)
(import queue [Queue])
(import time)
(import math)
(import re)
(import enum [Enum])

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

(defmacro if-match [string pattern pos-body neg-body]
  `(let [$ (re.match ~pattern ~string)]
     (if $
       ~pos-body
       ~neg-body)))

(defmacro when-macro [string pattern body]
  `(if-match ~string ~pattern
             (do
               ~@body)
             'None))

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
  (if (= **table**.state "betting")
    (user-temporary-balance uid)
    (user-main-balance uid)))

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
    (setv self.amount amount
          self.total total)
    (Exception.__init__ self))

  (defn __str__ [self]
    f"Cannot place bet for `${self.amount}` you only have `${total}` available"))

(defclass Task []
  (defn __init__ [self interval callback [repeating False]]
    (setv self.timer None
      self.interval interval
      self.callback callback
      self.running False
      self.repeating repeating
      self.start-time None))

  (defn run [self]
    (setv self.running False
      self.start-time (time.time))
    (self.callback)
    (when self.repeating
      (self.start)))

  (defn start [self]
    (when (not self.running)
      (setv self.timer (Timer self.interval self.run))
      (.start self.timer)
      (setv self.running True)))

  (defn elapsed-time [self]
    (if self.running
      (- (time.time) self.start-time)
      None))

  (defn stop [self]
    (.cancel self.timer)
    (setv self.running False)))

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
    (setv self.input string
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
          (yield #(amount (list (set numbers))))))))

(defclass Bet []
  (defn __init__ [self user amount numbers]
    (setv self.user user
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

(defclass Wheel []
  (defn __init__ [self]
    (setv self.spin-speed 1.0
          self.rotation 0.0
          self.camera (Camera2D)
          self.camera.target (Vector2 **half-wheel-size**.x 0)
          self.camera.offset (Vector2 (/ **window-size**.x 2) 0)
          self.camera.rotation 0.0
          self.camera.zoom 1.0))
  
  (defn draw-wheel-number [self n x]
    (let [w (int **wheel-size**.x)
          h (int **wheel-size**.y)
          istr (str (get **wheel-numbers** n))
          istr-width (/ (measure-text istr **font-size**) 2)]
      (draw-rectangle (int x) 0 w h (get **wheel-colors** n))
      (draw-rectangle-lines (int x) 0 w h GRAY)
      (draw-text istr (int (- (+ x **half-wheel-size**.x) istr-width)) (int (- **half-wheel-size**.y 15)) **font-size** WHITE)))
  
  (defn draw [self]
    (setv self.camera.target.x (+ self.camera.target.x **max-wheel-speed**))
    (when (> self.camera.target.x **max-wheel-size**)
      (setv self.camera.target.x (abs (- **max-wheel-size** self.camera.target.x))))
    (begin-mode-2d self.camera)
    (for [i (range 0 (len **wheel-colors**))]
      (draw-segment i (* i **wheel-size**.x)))
    (cond
      (< self.camera.target.x (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x)) (for [i (range (- (len **wheel-colors**) 1) (- (len **wheel-colors**) **max-visible-numbers**) -1)]
                                                                                                 (draw-segment i (neg (* (- (len **wheel-colors**) i) **wheel-size**.x))))
      (> self.camera.target.x (- **max-wheel-size** (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x))) (for [i (range 0 **max-visible-numbers**)]
                                                                                                                        (draw-segment i (+ **max-wheel-size** (* i **wheel-size**.x)))))
    (draw-line-ex (Vector2 self.camera.target.x 0) (Vector2 self.camera.target.x **wheel-size**.y) 2.0 WHITE)
    (end-mode-2d)))

(defclass Table []
  (setv states ["betting" "spin" "pause"])

  (defn __init__ [self]
    (setv self.machine (Machine :model self :states Table.states :initial "betting")
          self.task (Task 30 self.next-casino-state)
          self.wheel (Wheel))
    (.add-transition self.machine :trigger "next" :source "betting" :dest "spin")
    (.add-transition self.machine :trigger "next" :source "spin" :dest "pause")
    (.add-transition self.machine :trigger "next" :source "pause" :dest "betting")
    (.start self.task))

  (defn next-casino-state [self]
    (**table**.next))

  (defn draw-betting [self]
    (draw-text "Betting!" 5 5 20 LIME))

  (defn draw-spin [self]
    (draw-text "Spinning!" 5 5 20 ORANGE))

  (defn draw-pause [self]
    (draw-text "Waiting!" 5 5 20 RED))

  (defn draw [self]
    (.draw self.wheel)
    ((get [self.draw-betting self.draw-spin self.draw-pause] (.index self.states self.state))))

  (defn kill [self]
    (.stop self.task)))

(defn/a on-ready [event]
  (print "Twitch client is ready!")
  (await (event.chat.join-room **host-channel**)))

(defn/a on-register [cmd]
  (if-user-exist? cmd.user.id
                  (await (cmd.reply f"You're already registered..."))
                  (do
                    (.zadd **db** "leaderboard" {cmd.user.id 1000})
                    (await (cmd.reply f"You're now registered! You have a balance of $1000")))))

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
            (print (str bet))
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
      ; (toggle-fullscreen)
      (while (not (window-should-close))
        (begin-drawing)
        (clear-background WHITE)
        (.draw **table**)
        (end-drawing))
      (.kill **table**)
      (.stop chat)
      (await (.close twitch)))))

(defmacro if-color [c]
  `(lfor [x y] (enumerate **wheel-colors**)
         :if (= y ~c)
         x))

(setv **app-id** (read-file "twitch-token.txt")
      **app-secret** (read-file "twitch-secret.txt")
      **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT]
      **host-channel** "roryb_bellows"
      **window-size** (Vector2 1920 1080)
      **wheel-colors** [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED RED BLACK RED BLACK RED BLACK RED BLACK RED]
      **wheel-numbers** [0 32 15 19 4 21 2 25 17 34 6 27 13 36 11 30 8 23 10 5 24 16 33 1 20 14 31 9 22 18 29 7 28 12 35 3 26]
      **red** (if-color RED)
      **black** (if-color BLACK) 
      **wheel-size** (Vector2 100 100)
      **half-wheel-size** (Vector2 (/ **wheel-size**.x 2) (/ **wheel-size**.y 2))
      **max-wheel-speed** 10.0
      **max-wheel-size** (* (len **wheel-colors**) **wheel-size**.x)
      **max-visible-numbers** (math.ceil (/ **window-size**.x **wheel-size**.x))
      **visible-numbers-size** (* **max-visible-numbers** **wheel-size**.x)
      **font-size** 32
      **db** (redis.Redis "localhost" 6379 0)
      **bets** (Queue)
      **table** (Table))

(.run asyncio (run))