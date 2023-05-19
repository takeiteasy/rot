#!/usr/bin/env hy
(import socket)
(import json)
(import threading)
(import time)
(import queue)
(import re)

(defmacro unless [expr #*body]
	`(when (not ~expr)
		(do
            ~@body)))

(defmacro when-match [string pattern #* body]
	`(do
		(setv regex-matches (re.match ~pattern ~string))
		(if regex-matches
			(do
                ~@body)
            'None)))

(defmacro if-match [string pattern pos-body neg-body]
	`(do
		(setv regex-matches (re.match ~pattern ~string))
		(if regex-matches
			~pos-body
			~neg-body)))

(defn cond-regex [string #*args]
    (if args
        `(if-match string (get args 0)
            (get args 1)
            (cond-regex string (cut args 2 None)))
        'None))

(defmacro read-file [path]
	`(.strip (.read (open ~path "r"))))

(setv **token** (read-file "token.txt"))
(setv **host** (read-file "host.txt"))
(setv **help** (.split (read-file "help.txt") "\n"))
(setv work (queue.Queue))

(defclass InsufficientFundsError [Exception]
    (defn __init__ [self amount total]
        (setv self.amount amount)
        (setv self.total total)
        (Exception.__init__ self))
    
    (defn __str__ [self]
        (if (not self.total)
            f"Sorry, you're bust!"
            f"I can't place a bet for {self.amount}, you only have ${self.total}")))

(defclass Bet []
    (defn __init__ [self user amount numbers]
        (setv self.user user)
        (setv self.amount amount)
        (setv self.numbers numbers))
    
    (defn validate [self]
        ;; TODO: Find user's balance
        (setv tmp-total 0)
        (when (= tmp-total 0)
            (raise (InsufficientFundsError self.amount tmp-total)))
        (when (> self.amount tmp-total)
            (raise (InsufficientFundsError self.amount 0)))
        True))

(defclass IRCWorker [threading.Thread] 
	(defn __init__ [self] 
		(setv self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM)) 
		(setv self.host **host**) 
		(setv self.running False)
		(setv self.joined False)
		(threading.Thread.__init__ self))

	(defn run [self #*args #**kwargs]
		(self.sock.connect #("irc.chat.twitch.tv" 6667))
		(self.send "CAP REQ :twitch.tv/membership twitch.tv/tags twitch.tv/commands")
		(self.send f"PASS oauth:{**token**}")
		(self.send f"NICK {self.host}")
		(self.send f"JOIN #{self.host}")

		(setv self.running True)
		(while self.running
			(try
				(setv data (.decode (self.sock.recv 4069)))
				(unless data
					(setv self.running False)
					(return None))
				(setv lines (data.split "\n"))
				(for [line lines]
					(unless (not line)
						(setv result (self.process (line.strip)))
                        (when result
                            ;; TODO: Handle return of `process` here
                            `())))
				(except [KeyboardInterrupt]
					(setv self.running False)))))

	(defn send [self data]
		(print (+ "> " data))
		(self.sock.send (.encode (+ data "\n"))))

	(defn privmsg [self data]
		(self.send f"PRIVMSG #{self.host} :{data}"))

	(defn reply-to [self user msg]
		(self.privmsg f"@{user} {msg}"))

	(defn invalid-command [self user cmd]
		(self.reply-to user f"Sorry, I can't understand \"{cmd}\""))
	
    ;; TODO: Handle multiple bets delimed by comma, e.g. "x on y, w on z"
	(defn parse-bet [self user matches]
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
                        (do
                            (pdb.set_trace)
                            (self.invalid-command user bet-target)
                            None))
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
	
	(defn parse-privmsg [self matches]
		(setv user (get matches 2))
		(setv command (.split (get matches 4) " "))
		(setv first-word (get command 0))
		(cond
			(= first-word "!help")
			(do
                (for [line **help**]
				    (self.reply-to user line))
                None)
			(= first-word "!bet")
            (if-match (get matches 4) r"^!bet [£\$]?(\d+|\d{1,3}(,\d{3})*)(\.\d+)?\s(on )?(red|black|even|odd|((first|second|third|1st|2nd|3rd) (twelve|12))|((first|second|1st|2nd) half)|((first|second|third|1st|2nd|3rd) col(umn)?)|\d{1,2})$"
                (do
                    (setv bet-result (self.parse-bet user regex-matches))
                    (setv bet (Bet user (get bet-result 0) (get bet-result 1)))
                    (try
                        (bet.validate)
                        bet
                        (except [e InsufficientFundsError]
                            (do
                                (self.reply-to user (str e))
                                None))))
                (do
                    (print f"\"{(get matches 4)}\"")
                    (self.invalid-command user (get matches 4))
                    None))))

	(defn process [self line]
		(print (+ "< " line))
		(if (line.startswith "PING")
			(do 
                (self.send "PONG :tmi.twitch.tv")
                None)
			(if self.joined
                (when-match line f"^@(.*)\\s:(\\S+)!\\S+@\\S+\\.tmi\\.twitch\\.tv (\\S+) #{self.host} :(.*)$"
                    (when (and (.startswith (get regex-matches 4) "!") (= (get regex-matches 3) "PRIVMSG"))
                        (self.parse-privmsg regex-matches)))
				(when (= f":{self.host}!{self.host}@{self.host}.tmi.twitch.tv JOIN #{self.host}" line)
					(setv self.joined True)
                    None))))

	(defn kill [self]
		(setv self.running False)))

(setv irc-worker (IRCWorker))
(irc-worker.start)

;; (defn process [tree]
;; 	(print tree))

;; (with [sock (socket.socket socket.AF_INET socket.SOCK_STREAM)]
;; 	(sock.connect #("localhost" 1488))
;; 	(setv running True)
;; 	(work.put "{\"hello\": \"world\"}")
;; 	(while running
;; 		(try
;; 			(if (work.empty)
;; 				(time.sleep 0.1)
;; 				(do
;; 					(sock.send (.encode (work.get)))
;; 					(process (.decode (sock.recv 1024)))))
;; 			(except [e [socket.error json.decoder.JSONDecodeError]]
;; 				(print "ERROR: " (repr e))
;; 				(setv running False))
;; 			(except [KeyboardInterrupt]
;; 				(setv running False)))))

(irc-worker.kill)