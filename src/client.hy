#!/usr/bin/env hy
(import socket)
(import json)
(import threading)
(import time)
(import queue)
(import re)

(defmacro unless [expr body]
	`(if (not ~expr)
		~body
		`()))

(defmacro if-match [pattern string body]
	`(do
		(setv regex-matches (re.match ~pattern ~string))
		(if (not regex-matches)
			`()
			(do
				~body
				(return)))))

(setv **token** (.read (open "token.txt" "r")))
(setv work (queue.Queue))

(defclass IRCWorker [threading.Thread] 
	(defn __init__ [self] 
		(setv self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM)) 
		(setv self.host "bellowsroryb") 
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

	(defn process [self line]
		(print (+ "< " line))
		(if (= line "PING :tmi.twitch.tv")
			(self.send "PONG :tmi.twitch.tv")
			(if self.joined
				(do
					(if-match f"^@(.*)\\s:tmi\\.twitch\\.tv (\\S+)(\\s#{self.host})?" line
						(do 
							`()))
					(if-match f"^@(.*)\\s:{self.host}!{self.host}@{self.host}\\.tmi\\.twitch\\.tv (\\S+) #{self.host} :(.*)$" line
						(cond
							(= (get regex-matches 2) "PRIVMSG")
							(print (get regex-matches 3)))))
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