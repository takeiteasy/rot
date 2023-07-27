#!/usr/bin/env hy
(import threading)
(import socket)
(import json)
(import queue)
(import time)
(import pyray *)

(setv **requests** (queue.Queue))

(defclass SocketWorker [threading.Thread]
  (defn __init__ [self]
    (setv self.sock (socket.socket socket.AF_INET socket.SOCK_STREAM))
    (setv self.running False)
    (threading.Thread.__init__ self))

  (defn process [self response]
    (print response))

  (defn run [self #* args #** kwargs]
    (self.sock.connect #("localhost" 5432))
    (setv self.running True)
    (while self.running
      (try
        (if (.empty **requests**)
          (time.sleep 0.1)
          (do
            (self.sock.send (.encode (.get **requests**)))
            (self.process (.decode (self.sock.recv 1024)))))
        (except [e [socket.error json.decoder.JSONDecodeError]]
                (print "ERROR: " (repr e))
                (setv self.running False))
        (except [KeyboardInterrupt]
                (setv self.running False)))))

  (defn kill [self]
    (when self.running
      (self.sock.close)
      (setv self.running False))))

(setv socket-worker (SocketWorker))
(.start socket-worker)

; (init_window 1920 1080 "Roulette")
; (while (not (window_should_close))
;   (begin_drawing)
;   (clear_background WHITE)
;   (draw_text "Hello, world!" 5 5 20 VIOLET)
;   (end_drawing))

; (.kill socket-worker)