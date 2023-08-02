(import threading [Timer Thread])
(import asyncio)
(import queue)
(import time)
(import math)
(import pyray *)
(import transitions [Machine])
(import re)
(import redis)
(import twitchAPI [Twitch])
(import twitchAPI.oauth [UserAuthenticator])
(import twitchAPI.types [AuthScope ChatEvent])
(import twitchAPI.chat [Chat EventData ChatMessage ChatSub ChatCommand])

(defmacro percent [a b]
  `(* (/ ~a ~b) 100))

(defmacro neg [n]
  `(- 0 ~n))

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(defmacro unless [expr #* body]
  `(when (not ~expr)
     (do
       ~@body)))

(defmacro if-match [string pattern pos-body neg-body]
  `(do
     (setv regex-matches (re.match ~pattern ~string))
     (if regex-matches
       ~pos-body
       ~neg-body)))

(defn cond-regex [string #* args]
  (if args
    `(if-match string (get args 0)
               (get args 1)
               (cond-regex string (cut args 2 None)))
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
  (if (= **casino**.state "betting")
    (user-temporary-balance uid)
    (user-main-balance uid)))

(defclass InvalidBet [Exception]
  (defn __init__ [self [reason None]]
    (setv self.reason reason)
    (Exception.__init__ self))

  (defn bet-format [self]
    "!bet command format: `!bet [amount] [position]`")

  (defn __str__ [self]
    (if self.reason
      (.join " -- " [f"Sorry, I can't understand \"{self.reason}\"" (self.bet-format)])
      (self.bet-format))))

(defclass InvalidUser [Exception]
  (defn __str__ [self]
    "You are not registered, type `!register` to start!"))

(defclass Bet []
  (defn __init__ [self user amount numbers]
    (setv self.user user)
    (setv self.amount amount)
    (setv self.numbers numbers))

  (defn validate [self]
    (let [balance (user-balance self.user)]
      (if (not balance)
        (raise InvalidUser)
        (if (> balance self.amount)
          balance
          (raise (InsufficientFundsError self.amount balance)))))))

(defclass InsufficientFundsError [Exception]
  (defn __init__ [self amount total]
    (setv self.amount amount)
    (setv self.total total)
    (Exception.__init__ self))

  (defn __str__ [self]
    f"Cannot place bet for `${self.amount}` you only have `${total}` available"))

(defclass Task []
  (defn __init__ [self interval callback]
    (setv self.timer None
          self.interval interval
          self.callback callback
          self.running False
          self.start-time None))

  (defn run [self]
    (setv self.running False
          self.start-time (time.time))
    (self.start)
    (self.callback))

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

(defn resolve-bets [winner]
  (while (not (.empty **bets**))
    (let [bet (.get **bets**)]
      (when (in winner bet.numbers)
        (.zadd **db** "leaderboard:tmp" {bet.user (user-temporary-balance bet.user)}))))
  (for [balance (.zrange **db** "leaderboard:tmp" 0 -1 True)]
    (.zadd **db** "leaderboard" {bet.user (user-temporary-balance bet.user)}))
  (.delete **db** "leaderboard:tmp"))

(defclass Wheel [Task]
  (setv states ["spinning" "slowing" "stopped"])

  (defn __init__ [self]
    (setv self.machine (Machine :model self :states Wheel.states :initial "spinning")
          self.spin-speed 1.0
          self.rotation 0.0
          self.camera (Camera2D)
          self.camera.offset (Vector2 0 0)
          self.camera.target (Vector2 0 0)
          self.camera.rotation 0.0
          self.camera.zoom 1.0)
    (.add-transition self.machine :trigger "next" :source "spinning" :dest "slowing")
    (.add-transition self.machine :trigger "next" :source "slowing" :dest "stopped")
    (.add-transition self.machine :trigger "next" :source "stopped" :dest "spinning")
    (.add-transition self.machine :trigger "reset" :source "*" :dest "spinning")
    ; (Task.__init__ self 10 self.next-wheel-state)
    )

  (defn next-wheel-state [self]
    (self.next))
  
  (defn draw-wheel-number [self n x]
    (let [w (int **wheel-size**.x)
          h (int **wheel-size**.y)
          istr (str n)
          istr-width (/ (measure-text istr **font-size**) 2)]
      (draw-rectangle (int x) 0 w h (get **wheel-numbers** n))
      (draw-rectangle-lines (int x) 0 w h GRAY)
      (draw-text istr (int (- (+ x **half-wheel-size**.x) istr-width)) (int (- **half-wheel-size**.y 15)) **font-size** WHITE)))
  
  (defn render [self]
   (setv self.camera.target.x (+ self.camera.target.x **max-wheel-speed**))
    (when (> self.camera.target.x **max-wheel-size**)
      (setv self.camera.target.x (abs (- **max-wheel-size** self.camera.target.x))))
    (begin-mode-2d self.camera)
    (for [i (range 0 (len **wheel-numbers**))]
      (self.draw-wheel-number i (* i **wheel-size**.x)))
    (cond
      (< self.camera.target.x (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x)) (for [i (range (- (len **wheel-numbers**) 1) (- (len **wheel-numbers**) **max-visible-numbers**) -1)]
                                                                                                 (self.draw-wheel-number i (neg (* (- (len **wheel-numbers**) i) **wheel-size**.x))))
      (> self.camera.target.x (- **max-wheel-size** (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x))) (for [i (range 0 **max-visible-numbers**)]
                                                                                                                        (self.draw-wheel-number i (+ **max-wheel-size** (* i **wheel-size**.x)))))
    (draw-line-ex (Vector2 self.camera.target.x 0) (Vector2 self.camera.target.x **wheel-size**.y) 2.0 WHITE)
    (end-mode-2d)))

(defclass Table [Task]
  (setv states ["betting" "prespin" "spin" "pause"])

  (defn __init__ [self]
    (setv self.machine (Machine :model self :states Table.states :initial "betting")
          self.task (Task 30 self.next-casino-state)
          self.wheel (Wheel))
    (.add-transition self.machine :trigger "next" :source "betting" :dest "prespin")
    (.add-transition self.machine :trigger "next" :source "prespin" :dest "spin")
    (.add-transition self.machine :trigger "next" :source "spin" :dest "pause")
    (.add-transition self.machine :trigger "next" :source "pause" :dest "betting")
    (.start self.task))

  (defn next-casino-state [self]
    (**table**.next))

  (defn render-betting [self]
    (draw-text "Betting!" 5 5 20 LIME))

  (defn render-prespin [self]
    (draw-text "Pre-spin!" 5 5 20 YELLOW))

  (defn render-spin [self]
    (draw-text "Spinning!" 5 5 20 ORANGE))

  (defn render-pause [self]
    (draw-text "Waiting!" 5 5 20 RED))

  (defn render [self]
    (.render self.wheel)
    ((get [self.render-betting self.render-prespin self.render-spin self.render-pause] (.index self.states self.state))))

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

(defn parse-bet [matches]
  (setv bet-value (re.sub r"[^\d]" "" (get matches 1)))
  (setv bet-target (get matches 5))
  (setv bet-numbers
        (cond
          (in " " bet-target)
          (do
            (setv words (bet-target.split " "))
            (setv first-word
                  (cond-regex (.lower (get words 0))
                              r"^(first/1st)$"    '1
                              r"^(second/2nd)$"   '2
                              r"^(third/3rd)$"    '3))
            (unless first-word
                    (assert False))
            (cond-regex (.lower (get words 1))
                        "^half$"            (match first-word
                                              1 (range 1 19)
                                              2 (range 19 37))
                        r"^col(umn)?$"      (match first-word
                                               1 (range 1 37 3)
                                               2 (range 2 37 3)
                                               3 (range 3 37 3))
                        r"^(twelve|12)$"    (match first-word
                                               1 (range 1  13)
                                               2 (range 13 35)
                                               3 (range 25 37))))
          ;; TODO: Check for numerous bets and ranges, e.g. "x,y,z" "i-n"
          (re.match r"^(\d+)$" bet-target)
          (do
            (setv n (int bet-target))
            (if (and (>= n 0) (<= n 36))
              [n]
              None))
          True
          (do
            (setv string (bet-target.lower))
            (cond
              (= "red" string)    (range 2 37 2)
              (= "black" string)  (range 1 37 2)
              (= "even" string)   (range 2 37 2)
              (= "odd" string)    (range 1 37 2)))))
  #(bet-value bet-numbers))

(defn/a on-bet [cmd]
  (try
    (if (= (len cmd.parameter) 0)
      (raise (InvalidBet))
      (if-match cmd.parameter r"^[£\$]?(\d+|\d{1,3}(,\d{3})*)(\.\d+)?\s(on )?(red|black|even|odd|((first|second|third|1st|2nd|3rd) (twelve|12))|((first|second|1st|2nd) half)|((first|second|third|1st|2nd|3rd) col(umn)?)|\d{1,2})$"
                (let [bet-result (parse-bet regex-matches)
                      bet (Bet cmd.user.id (int (get bet-result 0)) (get bet-result 1))
                      balance (user-balance cmd.user.id)]
                  (let [balance (- (.validate bet) bet.amount)]
                    (do
                      (.put **bets** bet)
                      (.zadd **db** "leaderboard:tmp" {cmd.user.id balance})
                      (await (cmd.reply f"Bet placed `{cmd.parameter}` you have ${balance} available")))))
                (raise (InvalidBet cmd.parameter))))
    (except [e [InvalidBet InsufficientFundsError InvalidUser]]
            (await (cmd.reply (str e))))))

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
      (do
        (.delete **db** "leaderboard:tmp")
        (.register-event chat ChatEvent.READY on-ready)
        ; (.register-event chat ChatEvent.RAID on-raid)
        (.register-command chat "register" on-register)
        (.register-command chat "bet" on-bet)
        (.register-command chat "balance" on-balance)
        (.start chat) 

        (init-window 640 480 "Roulette")
        (set-target-fps 60)
        (while (not (window-should-close))
          (begin-drawing)
          (clear-background WHITE)
          (.render **table**)
          (end-drawing))
        (.kill **table**)
        (.stop chat)
        (await (.close twitch))))))

(setv **app-id** (read-file "twitch-token.txt")
      **app-secret** (read-file "twitch-secret.txt")
      **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT]
      **host-channel** "roryb_bellows"
      **addr** #("localhost" 5432)
      **db** (redis.Redis "localhost" 6379 0)
      **bets** (queue.Queue)
      **table** (Table)
      **window-size** (Vector2 1920 1080)
      **wheel-numbers** [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED]
      **wheel-size** (Vector2 100 100)
      **half-wheel-size** (Vector2 (/ **wheel-size**.x 2) (/ **wheel-size**.y 2))
      **max-wheel-speed** 50.0
      **max-wheel-size** (* (len **wheel-numbers**) **wheel-size**.x)
      **max-visible-numbers** (math.ceil (/ **window-size**.x **wheel-size**.x))
      **visible-numbers-size** (* **max-visible-numbers** **wheel-size**.x)
      **font-size** 32)

(.run asyncio (run))