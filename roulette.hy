(import socket)
(import json)
(import threading [Timer Thread])
(import asyncio)
(import queue)
(import time)
(import pyray *)
(import transitions [Machine])
(import re)
(import redis)
(import twitchAPI [Twitch])
(import twitchAPI.oauth [UserAuthenticator])
(import twitchAPI.types [AuthScope ChatEvent])
(import twitchAPI.chat [Chat EventData ChatMessage ChatSub ChatCommand])

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(setv **app-id** (read-file "twitch-token.txt"))
(setv **app-secret** (read-file "twitch-secret.txt"))
(setv **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT])
(setv **host-channel** "roryb_bellows")
(setv **db** (redis.Redis 'localhost' 6379 0))
(setv **placing-bets** True)
(setv **bets** (queue.Queue))
(setv **addr** #("localhost" 5432))

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

(defclass Server [Thread]
  (defn __init__ [self addr]
    (Thread.__init__ self)
    (setv self.addr addr
          self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM)
          self.clients []
          self.running False)
    (.setsockopt self.sock socket.SOL_SOCKET socket.SO_REUSEADDR 1))

  (defn remove-client [self client]
    (if (in client self.clients)
      (.close client)
      (.remove self.clients client)))

  (defn broadcast [self sender data]
    (for [client self.clients]
      (when (not (= sender client))
        (try
          (.sendall client data)
          (except [e Exception]
                  (print f"ERROR: {(repr e)}")
                  (self.remove-client client))))))

  (defn client-worker [self client]
    (while self.running
      (try
        (let [data (.recv client 1024)]
          (if data
            (self.broadcast client data)
            (time.sleep 0.1)))
        (except [e Exception]
                (print f"ERROR: {(repr e)}")
                (self.remove-client client)))))

  (defn run [self #* args #** kwargs]
    (.bind self.sock self.addr)
    (.listen self.sock 10)
    (setv self.running True)
    (while self.running
      (let [[client client-addr] (.accept self.sock)
            thread (Thread :target self.client-worker :args #(client))]
        (.append self.clients client)
        (.start thread)))))

(defclass Client [Thread]
  (defn __init__ [self addr]
    (Thread.__init__ self)
    (setv self.addr addr
          self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM)
          self.running False
          self.send-queue (queue.Queue)
          self.recv-queue (queue.Queue)))

  (defn send-queue-next? [self]
    (if (.empty self.send-queue)
      None
      (.get self.send-queue)))

  (defn recv-queue-next? [self]
    (if (.empty self.recv-queue)
      None
      (.get self.recv-queue)))

  (defn received [self]
    (let [result []
          msg (self.recv-queue-next?)]
      (while msg
        (.append result msg)
        (setv msg (self.recv-queue-next?)))
      result))

  (defn send [self msg]
    (.put self.send-queue (.encode msg)))

  (defn run [self #* args #** kwargs]
    (.connect self.sock self.addr)
    (setv self.running True)
    (while self.running
      (let [msg (self.send-queue-next?)]
        (while msg
          (try
            (.sendall self.sock msg)
            (except [e Exception]
                    (print f"ERROR: {(repr e)}")
                    (setv self.running False)))
          (setv msg (self.send-queue-next?))))
      (let [data (.recv self.sock 1024)]
        (.put self.recv-queue (.decode data)))
      (time.sleep 0.1))))

(defclass Task []
  (defn __init__ [self interval callback]
    (setv self.timer None
          self.interval interval
          self.callback callback
          self.running False)
    (self.start))

  (defn run [self]
    (setv self.running False)
    (self.start)
    (self.callback #* self.args #** self.kwargs))

  (defn start [self]
    (when (not self.running)
      (setv self.timer (Timer self.interval self.run))
      (.start self.timer)
      (setv self.running True)))

  (defn stop [self]
    (.cancel self.timer)
    (setv self.running False)))

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

(defmacro user-exist? [uid [table "leaderboard"]]
  `(not (= (.zscore **db** ~table ~uid) None)))

(defmacro if-user-exist? [uid pos-body neg-body]
  `(do
     (if (user-exist? ~uid)
       ~pos-body
       ~neg-body)))

(defmacro clear-temporary-leaderboard []
  `(.delete **db** "leaderboard:tmp"))

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
  (if **placing-bets**
    (user-temporary-balance uid)
    (user-main-balance uid)))

(defclass Bet []
  (defn __init__ [self user amount numbers]
    (setv self.user user)
    (setv self.amount amount)
    (setv self.numbers numbers)
    (setv self.success False))

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

(defn toggle-betting []
  (setv **placing-bets** (not **placing-bets**)))

(defn resolve-bets [winner]
  (while (not (.empty **bets**))
    (let [bet (.get **bets**)]
      (when (in winner bet.numbers)
        (.zadd **db** "leaderboard:tmp" {bet.user (user-temporary-balance bet.user)}))))
  (for [balance (.zrange **db** "leaderboard:tmp" 0 -1 True)]
    (.zadd **db** "leaderboard" {bet.user (user-temporary-balance bet.user)}))
  (clear-temporary-leaderboard))

(defn process-command [cmd]
  (print cmd)
  "{\"hello\": \"world\"}")

(defclass Casino [Thread]
  (defn __init__ [self addr]
    (Thread.__init__ self)
    (setv self.server (Server addr)
          self.front-client (Client addr)
          self.back-client (Client addr)
          self.running True)
    (Thread.start self))

  (defn run [self #* args #** kwargs]
    (while self.running
      (time.sleep 0.1))))

(defn/a run []
  (let [twitch (await (Twitch **app-id** **app-secret**))
        auth (UserAuthenticator twitch **user-scope**)
        tokens (await (.authenticate auth))]
    (await (.set-user-authentication twitch (get tokens 0) **user-scope** (get tokens 1)))
    (let [casino (Casino **addr**)
          chat (await (Chat twitch))]
      (do
        (clear-temporary-leaderboard)
        (.register-event chat ChatEvent.READY on-ready)
        ; (.register-event chat ChatEvent.RAID on-raid)
        (.register-command chat "register" on-register)
        (.register-command chat "bet" on-bet)
        (.register-command chat "balance" on-balance)
        (.start chat)

        (init-window 640 480 "Roulette")
        (while (not (window-should-close))
          (begin-drawing)
          (clear-background WHITE)
          (draw-text "Hello, world!" 5 5 20 VIOLET)
          (end-drawing))
        (.stop chat)
        (await (.close twitch))))))

(.run asyncio (run))