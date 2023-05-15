#!/usr/bin/env hy
(import re)

(setv joined False)
(setv host "bellowsroryb")

(defn stage1 [msg]
	(global joined)
	(if (= f ":{host}!{host}@{host}.tmi.twitch.tv JOIN #{host}" msg)
		(setv joined True)
		`()))

(defmacro if-match [pattern string body]
	`(do
		 (setv regex-matches (re.match ~pattern ~string))
		 (if (not regex-matches)
			 `()
			 ~body)))

(defn stage2 [msg]
	(if-match r "^@(.*)\s:tmi\.twitch\.tv (\S+)(\s#bellowsroryb)?" msg
						(do
							`()))
	(if-match r "^@(.*)\s:bellowsroryb!bellowsroryb@bellowsroryb\.tmi\.twitch\.tv (\S+) #bellowsroryb :(.*)$" msg
						(do
							(print (get regex-matches 3)))))

(with [file (open "log.txt")]
			(for [line file]
				(setv clean-line (line.strip))
				(if (not joined)
					(stage1 clean-line)
					(stage2 clean-line))))