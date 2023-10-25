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