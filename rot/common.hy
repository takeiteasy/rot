(import threading [Timer Thread])
(import time)

(defn percent [a b]
  (* (/ a b) 100))

(defn negative [n]
  (if (< n 0)
    n
    (- 0 n)))

(defmacro zero? [x pos-body neg-body]
  `(if (= ~x 0)
     ~pos-body
     ~neg-body))

(defn read-file [path]
  (.strip (.read (open path "r"))))

(defmacro unless [expr #* body]
  `(when (not ~expr)
     (do
       ~@body)))

(defn uniq [a]
  (list (set a)))

(defn clamp [n mi ma]
  (max (min n ma) mi))

(defn remap [v low1 high1 low2 high2]
  (+ low2 (* (- v low1) (/ (- high2 low2) (- high1 low1)))))

(defmacro if-match [string pattern pos-body neg-body]
  `(let [$ (re.match ~pattern ~string)]
     (if $
       ~pos-body
       ~neg-body)))

(defclass Task []
  (defn __init__ [self interval callback [start False] [repeating False]]
    (setv
      self.timer None
      self.interval interval
      self.callback callback
      self.running False
      self.repeating repeating
      self.start-time None)
    (when start
      (self.start)))

  (defn run [self]
    (setv self.running False)
    (self.callback)
    (when self.repeating
      (self.start)))

  (defn start [self]
    (when (not self.running)
      (setv
        self.timer (Timer self.interval self.run)
        self.start-time (time.time)
        self.running True)
      (.start self.timer)))

  (defn elapsed-time [self]
    (if self.running
      (- (time.time) self.start-time)
      0))

  (defn remaining-time [self]
    (if self.running
      (- self.interval (self.elapsed-time))
      0))

  (defn stop [self]
    (.cancel self.timer)
    (setv self.running False)))

(defclass Animation []
  (defn __init__ [self callback timeout]
    (setv
      self.timeout timeout
      self.task (Task timeout self.end)
      self.callback callback
      self.running False
      self.thread None))

  (defn start-thread [self]
    (let [t (Thread :target self.loop)]
      (setv self.thread t)
      (.start self.thread)))

  (defn loop [self]
    (while self.running
      (self.callback (.elapsed-time self.task) self.timeout)
      (time.sleep 0.1)))

  (defn begin [self]
    (if self.running
      (do
        (.stop self.task)
        (setv self.running False)
        (.begin self))
      (do
        (setv self.running True)
        (.start self.task)
        (.start-thread self))))

  (defn end [self]
    (when self.running
      (setv self.running False)
      (.stop self.task))))