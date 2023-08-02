(import pyray *)
(import random)
(import math)

(defmacro percent [a b]
  `(* (/ ~a ~b) 100))

(defmacro neg [n]
  `(- 0 ~n))

(setv window-size (Vector2 800 600)
      wheel-numbers [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK BLACK RED BLACK RED BLACK RED BLACK RED]
      wheel-size (Vector2 100 100)
      half-wheel-size (Vector2 (/ wheel-size.x 2) (/ wheel-size.y 2))
      max-wheel-speed 5.0
      max-wheel-size (* (len wheel-numbers) wheel-size.x)
      max-visible-numbers (math.ceil (/ window-size.x wheel-size.x))
      visible-numbers-size (* max-visible-numbers wheel-size.x)
      camera (Camera2D))

(init-window (int window-size.x) (int window-size.y) "test")
(set-target-fps 60)

(setv camera.target (Vector2 (- max-wheel-size 1000) 0)
      camera.offset (Vector2 (/ window-size.x 2) 0)
      camera.rotation 0.0
      camera.zoom 1.0)

(defn draw-wheel []
  (setv camera.target.x (+ camera.target.x max-wheel-speed))
  (when (> camera.target.x max-wheel-size)
    (setv camera.target.x (abs (- max-wheel-size camera.target.x))))
  (begin-mode-2d camera)
  (for [[i n] (enumerate wheel-numbers)]
    (let [x (int (* i wheel-size.x))
          w (int wheel-size.x)
          h (int wheel-size.y)
          istr (str i)
          font-size 32
          istr-width (/ (measure-text istr font-size) 2)]
      (draw-rectangle x 0 w h n)
      (draw-rectangle-lines x 0 w h WHITE)
      (draw-text istr (int (- (+ (* i wheel-size.x) half-wheel-size.x) istr-width)) (int (- half-wheel-size.y 15)) font-size WHITE)))
  (draw-line-ex (Vector2 camera.target.x 0) (Vector2 camera.target.x wheel-size.y) 2.0 WHITE)
  (cond
    (< camera.target.x (* (math.floor (/ max-visible-numbers 2)) wheel-size.x)) (for [i (range (- (len wheel-numbers) 1) (- (len wheel-numbers) max-visible-numbers) -1)]
                                                                                  (let [x (int (neg (* (- (len wheel-numbers) i) wheel-size.x)))
                                                                                        w (int wheel-size.x)
                                                                                        h (int wheel-size.y)
                                                                                        istr (str i)
                                                                                        font-size 32
                                                                                        istr-width (/ (measure-text istr font-size) 2)]
                                                                                    (draw-rectangle x 0 w h (get wheel-numbers i))
                                                                                    (draw-rectangle-lines x 0 w h WHITE)
                                                                                    (draw-text istr (int (- (+ x half-wheel-size.x) istr-width)) (int (- half-wheel-size.y 15)) font-size WHITE)))
    (> camera.target.x (- max-wheel-size (* (math.floor (/ max-visible-numbers 2)) wheel-size.x))) (for [i (range 0 max-visible-numbers)]
                                                                                                     (let [x (int (+ max-wheel-size (* i wheel-size.x)))
                                                                                                           w (int wheel-size.x)
                                                                                                           h (int wheel-size.y)
                                                                                                           istr (str i)
                                                                                                           font-size 32
                                                                                                           istr-width (/ (measure-text istr font-size) 2)]
                                                                                                       (draw-rectangle x 0 w h (get wheel-numbers i))
                                                                                                       (draw-rectangle-lines x 0 w h WHITE)
                                                                                                       (draw-text istr (int (- (+ x half-wheel-size.x) istr-width)) (int (- half-wheel-size.y 15)) font-size WHITE))))
  (end-mode-2d))

(while (not (window-should-close))
  (begin-drawing)
  (clear-background BLACK)
  (draw-wheel)
  (end-drawing))
(close-window)