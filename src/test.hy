#!/usr/bin/env hy

(setv joined False)
(setv host "bellowsroryb")

(defn stage1 [msg]
  (global joined)
;   (print (+ "STAGE1: " msg))
  (if (= f":{host}!{host}@{host}.tmi.twitch.tv JOIN #{host}" msg)
    (setv joined True)
    `()))

(defn stage2 [msg]
  (print msg))

(with [file (open "log.txt")]
      (for [line file]
        (setv clean-line (line.strip))
        (if (not joined)
          (stage1 clean-line)
          (stage2 clean-line))))