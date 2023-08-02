(import pyray *)
(import random)
(import math)

(defmacro percent [a b]
  `(* (/ ~a ~b) 100))

(defmacro neg [n]
  `(- 0 ~n))

(setv window-size (Vector2 1920 1080)
      wheel-numbers [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED]
      wheel-size (Vector2 100 100)
      half-wheel-size (Vector2 (/ wheel-size.x 2) (/ wheel-size.y 2))
      max-wheel-speed 50.0
      max-wheel-size (* (len wheel-numbers) wheel-size.x)
      max-visible-numbers (math.ceil (/ window-size.x wheel-size.x))
      visible-numbers-size (* max-visible-numbers wheel-size.x)
      font-size 32
      camera (Camera2D))

(init-window (int window-size.x) (int window-size.y) "test")
(set-target-fps 60)

(setv camera.target (Vector2 half-wheel-size.x 0)
      camera.offset (Vector2 (/ window-size.x 2) 0)
      camera.rotation 0.0
      camera.zoom 1.0)

(defn draw-wheel-number [n x]
  (let [w (int wheel-size.x)
        h (int wheel-size.y)
        istr (str n)
        istr-width (/ (measure-text istr font-size) 2)]
    (draw-rectangle (int x) 0 w h (get wheel-numbers n))
    (draw-rectangle-lines (int x) 0 w h GRAY)
    (draw-text istr (int (- (+ x half-wheel-size.x) istr-width)) (int (- half-wheel-size.y 15)) font-size WHITE)))

(defn draw-wheel []
  (setv camera.target.x (+ camera.target.x max-wheel-speed))
  (when (> camera.target.x max-wheel-size)
    (setv camera.target.x (abs (- max-wheel-size camera.target.x))))
  (begin-mode-2d camera)
  (for [[i n] (enumerate wheel-numbers)]
    (draw-wheel-number i (* i wheel-size.x)))
  (cond
    (< camera.target.x (* (math.floor (/ max-visible-numbers 2)) wheel-size.x)) (for [i (range (- (len wheel-numbers) 1) (- (len wheel-numbers) max-visible-numbers) -1)]
                                                                                  (draw-wheel-number i (neg (* (- (len wheel-numbers) i) wheel-size.x))))
    (> camera.target.x (- max-wheel-size (* (math.floor (/ max-visible-numbers 2)) wheel-size.x))) (for [i (range 0 max-visible-numbers)]
                                                                                                     (draw-wheel-number i (+ max-wheel-size (* i wheel-size.x)))))
  (draw-line-ex (Vector2 camera.target.x 0) (Vector2 camera.target.x wheel-size.y) 2.0 WHITE)
  (end-mode-2d))

(while (not (window-should-close))
  (begin-drawing)
  (clear-background BLACK)
  (draw-wheel)
  (end-drawing))
(close-window)