#!/usr/bin/env hy
(import socket)
(import json)
(import threading)
(import time)
(import queue)
(import pyray *)

(defclass Server [threading.Thread]
    (defn __init__ [self port]
        (setv self.port port)
        (setv self.running False)
        (threading.Thread.__init__ self))

    (defn run [self #*args #**kwargs]
        (setv self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM))
        (self.sock.bind #("localhost" self.port))
        (self.sock.listen 1)
        (setv self.client (get (self.sock.accept) 0))
        (setv self.running True)
        (while self.running
            (try
                (self.process (json.loads (.decode (self.client.recv 1024))))
                (time.sleep 0.1)
                (except [e [socket.error json.decoder.JSONDecodeError]]
                    (print "ERROR: " (repr e))
                    (setv self.running False))
                (except [KeyboardInterrupt]
                    (setv self.running False)))))
    
    (defn connected [self]
        self.running)
    
    (defn process [self tree]
        (if (in "error" tree)
            False
            (do
                (print (json.dumps tree))
                (self.client.send (.encode "{\"hello\": \"world\"}"))
                True)))
    
    (defn kill [self]
        (self.client.close)
        (self.sock.close)
        (setv self.running False)))

(setv server (Server 1488))
(server.start)

;; (while (not (server.connected))
;;     (time.sleep 0.1))

;; (init_window 1920 1080 "Roulette")
;; (while (and (not (window_should_close)) (server.connected))
;;     (begin_drawing)
;;     (clear_background WHITE)
;;     (draw_text "Hello, world!" 5 5 20 VIOLET)
;;     (end_drawing))

;; (close_window)
;; (server.kill)