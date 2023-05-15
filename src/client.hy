#!/usr/bin/env hy
(import socket)
(import json)
(import threading)
(import time)
(import queue)

(defmacro unless [expr body]
    `(if (not ~expr)
        ~body
        `()))

(setv **token** (.read (open "token.txt" "r")))
(setv work (queue.Queue))

(defclass IRCWorker [threading.Thread]
    (defn __init__ [self]
        (setv self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM))
        (setv self.host "bellowsroryb")
        (setv self.running False)
        (threading.Thread.__init__ self))
    
    (defn run [self #*args #**kwargs]
        (self.sock.connect #("irc.chat.twitch.tv" 6667))
        (self.send "CAP REQ :twitch.tv/membership twitch.tv/tags twitch.tv/commands")
        (self.send f"PASS oauth:{**token**}")
        (self.send f"NICK {self.host}")
        (self.send f"JOIN #{self.host}")
        (self.privmsg "Hello, world!")

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
                    (if (= line "PING :tmi.twitch.tv")
                        (self.send "PONG :tmi.twitch.tv")
                        (self.process line)))
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
    
    (defn process [self data]
        (print (+ "< " data))
        `())
    
    (defn kill [self]
        (setv self.running False)))

(setv irc-worker (IRCWorker))
(irc-worker.start)

;; (defn process [tree]
;;     (print tree))

;; (with [sock (socket.socket socket.AF_INET socket.SOCK_STREAM)]
;;     (sock.connect #("localhost" 1488))
;;     (setv running True)
;;     (work.put "{\"hello\": \"world\"}")
;;     (while running
;;         (try
;;             (if (work.empty)
;;                 (time.sleep 0.1)
;;                 (do
;;                     (sock.send (.encode (work.get)))
;;                     (process (.decode (sock.recv 1024)))))
;;             (except [e [socket.error json.decoder.JSONDecodeError]]
;;                 (print "ERROR: " (repr e))
;;                 (setv running False))
;;             (except [KeyboardInterrupt]
;;                 (setv running False)))))

(irc-worker.kill)