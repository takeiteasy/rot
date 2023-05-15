#!/usr/bin/env hy
(import socket)
(import json)
(import threading)
(import time)
(import queue)
(import re)

(defmacro *if [expr body]
	`(if ~expr
		~body
		`()))

(defmacro unless [expr body]
	`(*if (not ~expr)
		~body))

(defmacro *if-match [pattern string body]
	`(do
		(setv regex-matches (re.match ~pattern ~string))
		(*if regex-matches
			(do
				~body
				(return)))))

(defmacro if-match [pattern string pos-body neg-body]
	`(do
		(setv regex-matches (re.match ~pattern ~string))
		(if regex-matches
			(do
				~pos-body
				(return))
			(do
				~neg-body
				(return)))))

(defmacro read-file [path]
	`(.strip (.read (open ~path "r"))))

(setv **token** (read-file "token.txt"))
(setv **host** (read-file "host.txt"))
(setv **help** (.split (read-file "help.txt") "\n"))
(setv work (queue.Queue))

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
					(do
						(setv self.running False)
						(break)))
				(setv lines (data.split "\n"))
				(for [line lines]
					(unless (not line)
						(self.process (line.strip))))
				(except [e [socket.error json.decoder.JSONDecodeError]]
					(print "ERROR: " (repr e))
					(setv running False))
				(except [KeyboardInterrupt]
					(setv self.running False)))))

	(defn send [self data]
		(print (+ "> " data))
		(self.sock.send (.encode (+ data "\n"))))

	(defn privmsg [self data]
		(self.send f"PRIVMSG #{self.host} :{data}"))

	(defn reply-to [self user msg]
		(self.privmsg f"@{user} {msg}"))

	(defn invalid-command [self cmd]
		(self.privmsg f"Sorry, I can't understand \"{cmd}\""))
	
	(defn parse [self matches]
		(setv user (get matches 2))
		(*if (and (.startswith (get matches 4) "!") (= (get matches 3) "PRIVMSG"))
			(do
				(setv command (.split (get matches 4) " "))
				(setv first-word (get command 0))
				(cond
					(= first-word "!help")
					(for [line **help**]
						(self.reply-to user line))
					(= first-word "!bet")
					(do
						(assert (= (len command) 2))
						(if-match r"^!bet \$?(\d+|\d{1,3}(,\d{3})*)(\.\d+)?\s(on )?(red|black|even|odd|((first|second|third|1st|2nd|3rd) (twelve|12))|((first|second|1st|2nd) half)|((first|second|third|1st|2nd|3rd) col(umn)?)|\d{1,2})?" (get regex-matches 4)
							(do
								(setv bet (re.sub (get matches 1) r"[^\d]"))
								(setv bet-string f"{bet:,}")
								;; check/deduct funs
								;; update game/database
								(self.reply-to user f"Your ${(get matches 1)} bet has been placed!"))
							(self.invalid-command (get matches 4))))))))

	(defn process [self line]
		(print (+ "< " line))
		(if (.startswith line "PING")
			(self.send "PONG :tmi.twitch.tv")
			(if self.joined
				(do
					;; (*if-match f"^@(.*)\\s:tmi\\.twitch\\.tv (\\S+)(\\s#{self.host})?" line
					;; 	`())
					(*if-match f"^@(.*)\\s:(\\S+)!\\S+@\\S+\\.tmi\\.twitch\\.tv (\\S+) #{self.host} :(.*)$" line
						(self.parse regex-matches)))
				(if (= f":{self.host}!{self.host}@{self.host}.tmi.twitch.tv JOIN #{self.host}" line)
					(setv self.joined True)
					`()))))

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