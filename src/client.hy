#!/usr/bin/env hy
(import asyncio)
(import socket)
(import json)
(import re)
(import twitchAPI [Twitch])
(import twitchAPI.oauth [UserAuthenticator])
(import twitchAPI.types [AuthScope ChatEvent])
(import twitchAPI.chat [Chat EventData ChatMessage ChatSub ChatCommand])

(defmacro read-file [path]
  `(.strip (.read (open ~path "r"))))

(setv **app-id** (read-file "token.txt"))
(setv **app-secret** (read-file "secret.txt"))
(setv **user-scope** [AuthScope.CHAT_READ AuthScope.CHAT_EDIT])
(setv **host-channel** (read-file "host.txt"))

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

(defclass Bet []
  (defn __init__ [self user amount numbers]
    (setv self.user user)
    (setv self.amount amount)
    (setv self.numbers numbers))

  (defn validate [self]
    ;; TODO: Find user's balance
    (setv tmp-total 101)
    (when (= tmp-total 0)
      (raise (InsufficientFundsError self.amount tmp-total)))
    (when (> self.amount tmp-total)
      (raise (InsufficientFundsError self.amount 0)))
    True))

(defclass InsufficientFundsError [Exception]
  (defn __init__ [self amount total]
    (setv self.amount amount)
    (setv self.total total)
    (Exception.__init__ self))

  (defn __str__ [self]
    (if (not self.total)
      f"Sorry, you're bust!"
      f"I can't place a bet for {self.amount}, you only have ${self.total}")))

(defn/a on-ready [event]
  (print "TEST! FINALLY!")
  (await (event.chat.join-room **host-channel**)))

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
                              r "^(first/1st)$"    '1
                              r "^(second/2nd)$"   '2
                              r "^(third/3rd)$"    '3))
            (unless first-word
                    (assert False))
            (cond-regex (.lower (get words 1))
                        "^half$"            (match first-word
                                              1 (range 1 19)
                                              2 (range 19 37))
                        r "^col(umn)?$"      (match first-word
                                               1 (range 1 37 3)
                                               2 (range 2 37 3)
                                               3 (range 3 37 3))
                        r "^(twelve|12)$"    (match first-word
                                               1 (range 1  13)
                                               2 (range 13 35)
                                               3 (range 25 37))))
          ;; TODO: Check for numerous bets and ranges, e.g. "x,y,z" "i-n"
          (re.match r "^(\d+)$" bet-target)
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
                (do
                  (setv bet-result (parse-bet regex-matches))
                  (setv bet (Bet None (int (get bet-result 0)) (get bet-result 1)))
                  (try
                    (bet.validate)
                    
                    (except [e InsufficientFundsError]
                            (await (cmd.reply (str e))))))
                (raise (InvalidBet cmd.parameter))))
    (except [e InvalidBet]
            (await (cmd.reply (str e))))))

(defn/a run []
  (let [twitch (await (Twitch **app-id** **app-secret**))
        auth (UserAuthenticator twitch **user-scope**)
        tokens (await (.authenticate auth))]
    (await (.set-user-authentication twitch (get tokens 0) **user-scope** (get tokens 1)))
    (let [chat (await (Chat twitch))]
      (do
        (.register-event chat ChatEvent.READY on-ready)
        (.register-command chat "bet" on-bet)
        (.start chat)
        (try
          (input "Press ENTER to quit\n")
          (finally
            (.stop chat)
            (await (.close twitch)))))  None)))

(.run asyncio (run))