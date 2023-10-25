#!/usr/bin/env hy
(import asyncio)
(import re)
(import twitchAPI [Twitch])
(import twitchAPI.oauth [UserAuthenticator])
(import twitchAPI.types [AuthScope ChatEvent])
(import twitchAPI.chat [Chat EventData ChatMessage ChatSub ChatCommand])
(import rot.croupier *)

(defmacro if-color [c]
  `(lfor [x y] (enumerate **wheel-numbers**)
         :if (= (get **wheel-colors** x) ~c)
         y))

(setv
  **app-id** (read-file "twitch-token.txt")
  **app-secret** (read-file "twitch-secret.txt")
  **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT]
  **host-channel** "bellowsroryb"
  **red** (if-color RED)
  **black** (if-color BLACK)
  **croupier** (Croupier))

(defn/a on-ready [event]
  (print "Twitch client is ready!")
  (await (event.chat.join-room **host-channel**)))

(defn/a on-register [cmd]
  (let [player (find-user cmd.user.id)]
    (if player
      (await (cmd.reply "You are already registered...")) 
      (do
        (new-user cmd.user.id)
        (await (cmd.reply f"You are now registered, your balance is ${**default-balance**}"))))))

(defclass Bet []
  (defn __init__ [self user amount numbers multiplier]
    (setv
      self.user user
      self.amount amount
      self.numbers numbers
      self.multiplier multiplier))

  (defn __str__ [self]
    f"(Bet uid:{self.user} amount:{self.amount} numbers:{self.numbers} return:{(+ self.amount (* self.amount self.multiplier))})"))

(defmacro return-bet [numbers multiplier]
  `(return (Bet uid bet-amount ~numbers ~multiplier)))

(defmacro set-first-part [n]
  `(do
     (setv first-part ~n)
     (self.OnFirstPart)))

(defmacro valid-number? [n]
  `(and (>= ~n 0)
        (< (len **wheel-numbers**))))

(defmacro test-first-part [multiplier #* parts]
  `(let [n (match first-word ~@parts)]
    (if (not n)
      (raise (InvalidBet))
      (return-bet n ~multiplier))))

(defclass InvalidBet [Exception]
  (defn __init__ [self [reason None]]
    (setv self.reason reason)
    (Exception.__init__ self))

  (defn bet-format [self]
    "!bet command format: `!bet [amount] [position]`")

  (defn __str__ [self]
    (if self.reason
      f"Sorry, I can't understand \"{self.reason}\" -- {self.bet-format}"
      (self.bet-format))))

(defclass BetParser []
  (setv states ["ExpectAmount" "ExpectNumbers" "ExpectSecondPart"])

  (defn __init__ [self]
    (setv self.machine (Machine :model self :states BetParser.states :initial "ExpectAmount"))
    (.add-transition self.machine :trigger "OnAmount" :source "ExpectAmount" :dest "ExpectNumbers")
    (.add-transition self.machine :trigger "OnFirstPart" :source "ExpectNumbers" :dest "ExpectSecondPart"))
  
  (defn parse [self uid message]
    ; TODO: Add Streets 11:1, splits 17:1, corner 8:1, 6 line 5:1
    ; TODO: Multiple bets in one command?
    (let [tokens (lfor token (.split message " ") :if token (.lower token))
          bet-amount 0
          first-word None]
      (for [token tokens]
        (match self.state
          "ExpectAmount" (if-match token r"^\$?(\d+|\d{1,3}(,\d{3})*)$"
                                   (do
                                     (setv bet-amount (int (re.sub "[^0-9]" "" (get $ 1))))
                                     (self.OnAmount))
                                   (raise InvalidBet))
          "ExpectNumbers" (cond
                            (re.match r"^\d+$" token) (let [t (int token)]
                                                         (if (valid-number? t)
                                                           (return-bet [t] 35)
                                                           (raise (InvalidBet))))
                            (= token "red") (return-bet **red** 1)
                            (= token "black") (return-bet **black** 1)
                            (= token "even") (return-bet (list (range 2 37 2)) 1)
                            (= token "odd") (return-bet (list (range 1 37 2)) 1)
                            (= token "low") (return-bet (list (range 1 19)) 1)
                            (= token "high") (return-bet (list (range 19 37)) 1)
                            (or (= token "first")
                                (= token "1st")) (set-first-part 1)
                            (or (= token "second")
                                (= token "2st")) (set-first-part 2)
                            (or (= token "third")
                                (= token "3rd")) (set-first-part 3)
                            True (unless (= token "on")
                                         (raise (InvalidBet))))
          "ExpectSecondPart" (cond
                               (re.match r"^col(umn)?$" token) (test-first-part 2
                                                                 1 (range 1 37 3)
                                                                 2 (range 2 37 3)
                                                                 3 (range 3 37 3))
                               (re.match r"^(twelve|12|dozen)$" token) (test-first-part 2
                                                                         1 (range 1  13)
                                                                         2 (range 13 25)
                                                                         3 (range 25 37))
                               True (raise (InvalidBet))))))
      (raise (InvalidBet))))

(defclass InvalidUser [Exception]
  (defn __str__ [self]
    "You are not registered, type `!register` to start!"))

(defmacro if-valid-user [uid pos-body neg-body]
  `(let [player (find-user ~uid)]
     (if player
       ~pos-body
       ~neg-body)))

(defmacro when-valid-user [uid #* body]
  `(if-valid-user ~uid
                  ~@body
                  (raise InvalidUser)))

(defn current-user-stake [uid]
  (if (.hexists **cache** "stakes" uid)
    (int (.hget **cache** "stakes" uid))
    0))

(defn append-stake [uid amount]
  (if (.hexists **cache** "stakes" uid)
    (.hincrby **cache** "stakes" uid amount)
    (.hset **cache** "stakes" uid amount)))

(defclass InsufficientFundsError [Exception]
  (defn __init__ [self amount total]
    (setv
      self.amount amount
      self.total total)
    (Exception.__init__ self))

  (defn __str__ [self]
    f"Cannot place bet for `${self.amount}` you only have `${self.total}` available"))

(defn/a on-bet [cmd]
  (when (> (len cmd.parameter) 0)
    (if (= **croupier**.state "betting")
      (if-valid-user cmd.user.id
                     (try
                       (let [bet (.parse (BetParser) cmd.user.id cmd.parameter)
                             funds (- player.balance (current-user-stake player.uid))]
                         (if (> bet.amount funds)
                           (raise (InsufficientFundsError bet.amount funds))
                           (do
                             (append-stake player.uid bet.amount)
                             (.put **bets** bet)
                             (await (cmd.reply f"Your bet for ${bet.amount} has been placed!")))))
                       (except [e [InvalidBet InsufficientFundsError]]
                               (await (cmd.reply (str e)))))
                     (await (cmd.reply "You are not registered, please type `!register` to begin")))
      (await (cmd.reply f"Sorry, can't place a bet right now!")))))

(defn/a on-balance [cmd]
  (if-valid-user cmd.user.id
                 (let [stake (current-user-stake player.uid)]
                   (await (cmd.reply (zero? stake
                                            f"Your balance is ${player.balance}"
                                            f"Your total balance is ${player.balance} with ${(- player.balance stake)} available"))))
                 (await (cmd.reply "You are not registered, please type `!register` to begin"))))

(defn/a run []
  (let [twitch (await (Twitch **app-id** **app-secret**))
        auth (UserAuthenticator twitch **user-scope**)
        tokens (await (.authenticate auth))]
    (await (.set-user-authentication twitch (get tokens 0) **user-scope** (get tokens 1)))
    (let [chat (await (Chat twitch))]
      (connect-database)
      (clear-stakes)

      (.register-event chat ChatEvent.READY on-ready)
      ; (.register-event chat ChatEvent.RAID on-raid)
      (.register-command chat "register" on-register)
      (.register-command chat "bet" on-bet)
      (.register-command chat "balance" on-balance)
      (.start chat) 

      (init-window (int **window-size**.x) (int **window-size**.y) "Twitch Roulette")
      (set-target-fps 60)
      (.start **croupier**)
      ; (toggle-fullscreen)
      (while (not (window-should-close))
        (begin-drawing)
        (clear-background BLACK)
        (.draw **croupier**)
        (end-drawing))
      
      (.kill **croupier**)
      (.stop chat)
      (await (.close twitch)))))

(.run asyncio (run))