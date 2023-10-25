(import pyray *)
(import transitions [Machine])
(import math)
(import rot.common *)

(setv
  **window-size** (Vector2 1920 1080)
  **wheel-colors** [GREEN RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK RED BLACK]
  **wheel-numbers** [0 32 15 19 4 21 2 25 17 34 6 27 13 36 11 30 8 23 10 5 24 16 33 1 20 14 31 9 22 18 29 7 28 12 35 3 26]
  **wheel-size** (Vector2 300 300)
  **half-wheel-size** (Vector2 (/ **wheel-size**.x 2) (/ **wheel-size**.y 2))
  **max-wheel-speed** 100.0
  **max-wheel-size** (* (len **wheel-colors**) **wheel-size**.x)
  **max-visible-numbers** (math.ceil (/ **window-size**.x **wheel-size**.x))
  **visible-numbers-size** (* **max-visible-numbers** **wheel-size**.x)
  **font-size** 32
  **table-box-size** (Vector2 125 125))

(defmacro with-alpha [n c]
  `(if (= self.current-number ~n)
     ~c
     (color-alpha ~c self.alpha)))

(defclass Wheel []
  (setv states ["stopped" "spinning" "slowing"])

  (defn __init__ [self]
    (setv
      self.machine (Machine :model self :states Wheel.states :initial "stopped")
      self.speed 0.0
      self.camera (Camera2D)
      self.camera.target (Vector2 **half-wheel-size**.x 0)
      self.camera.offset (Vector2 (/ **window-size**.x 2) 0)
      self.camera.rotation 0.0
      self.camera.zoom 1.0
      self.current-number 0
      self.alpha 0.5)
    (.add-transition self.machine :trigger "stop" :source "*" :dest "stopped" :before "reset_speed")
    (.add-transition self.machine :trigger "reset" :source "*" :dest "stopped" :before "reset_speed_position")
    (.add-transition self.machine :trigger "start" :source "stopped" :dest "spinning")
    (.add-transition self.machine :trigger "slow" :source "spinning" :dest "slowing"))

  (defn reset-speed [self]
    (setv self.speed 0.0))

  (defn reset-speed-position [self]
    (setv
      self.speed 0.0
      self.camera.target (Vector2 **half-wheel-size**.x 0)))

  (defn draw-segment [self i x]
    (let [w (int **wheel-size**.x)
          h (int **wheel-size**.y)
          n (get **wheel-numbers** i)
          text (str n)]
      (draw-rectangle (int x) 0 w h (with-alpha n (get **wheel-colors** i)))
      (draw-rectangle-lines (int x) 0 w h (with-alpha n GRAY))
      (draw-text text (int (- (+ x **half-wheel-size**.x) (/ (measure-text text **font-size**) 2))) (int (- **half-wheel-size**.y 15)) **font-size** (with-alpha n WHITE))))

  (defn draw [self]
    (when (= self.state "slowing")
      (setv self.speed (- self.speed 0.1))
      (when (<= self.speed 1)
        (self.stop)))
    (setv self.camera.target.x (+ self.camera.target.x self.speed))
    (when (> self.camera.target.x **max-wheel-size**)
      (setv self.camera.target.x (abs (- **max-wheel-size** self.camera.target.x))))
    (setv self.current-number (get **wheel-numbers** (math.floor (/ self.camera.target.x **wheel-size**.x))))
    (begin-mode-2d self.camera)
    (for [i (range 0 (len **wheel-colors**))]
      (self.draw-segment i (* i **wheel-size**.x)))
    (let [half-width (* (math.floor (/ **max-visible-numbers** 2)) **wheel-size**.x)]
      (cond
        (< self.camera.target.x half-width) (for [i (range (- (len **wheel-colors**) 1) (- (len **wheel-colors**) **max-visible-numbers**) -1)]
                                              (self.draw-segment i (negative (* (- (len **wheel-colors**) i) **wheel-size**.x))))
        (> self.camera.target.x (- **max-wheel-size** half-width)) (for [i (range 0 **max-visible-numbers**)]
                                                                     (self.draw-segment i (+ **max-wheel-size** (* i **wheel-size**.x))))))
    (draw-line-ex (Vector2 self.camera.target.x 0) (Vector2 self.camera.target.x **wheel-size**.y) 2.0 (with-alpha -1 WHITE))
    (end-mode-2d)))

(defclass Table []
  (defn __init__ [self]
    (setv
      self.camera (Camera2D)
      self.camera.target (Vector2 0 0)
      self.camera.offset (Vector2 0 **wheel-size**.y)
      self.camera.rotation 0.0
      self.camera.zoom 1.0
      self.alpha 1.0))

  (defn draw-box [self x y sz col [border-col WHITE]]
    (draw-rectangle (int x) (int y) (int sz.x) (int sz.y) col)
    (draw-rectangle-lines (int x) (int y) (int sz.x) (int sz.y) border-col))

  (defn draw-text-box [self x y sz text col [border-col WHITE] [text-col WHITE]]
    (let [text-width (/ (measure-text text **font-size**) 2)]
      (self.draw-box x y sz col border-col)
      (draw-text text (int (+ x (- (/ sz.x 2) text-width))) (int (+ y (- (/ sz.y 2) 15))) **font-size** text-col)))

  (defn draw [self]
    (begin-mode-2d self.camera)
    (let [ox (int (- (/ **window-size**.x 2) (/ (* **table-box-size**.x 12) 2)))
          oy (int (* **table-box-size**.y 3))]
      (self.draw-text-box (- ox **table-box-size**.x) **table-box-size**.y (Vector2 (int **table-box-size**.x) oy) "0" GREEN)
      (for [[i text] (enumerate ["1st 12" "2nd 12" "3rd 12"])]
        (let [w (* **table-box-size**.x 4)
              fw (* **table-box-size**.x 12)]
          (self.draw-text-box (+ ox fw) (+ **table-box-size**.y (* **table-box-size**.y i)) **table-box-size** "2 to 1" GREEN)
          (self.draw-text-box (+ ox (* w i)) (+ oy **table-box-size**.y) (Vector2 w (/ **table-box-size**.y 2)) text GREEN)))
      (let [w (* **table-box-size**.x 2)
            h (/ **table-box-size**.y 2)
            sz (Vector2 w h)]
        (for [i (range 0 6)]
          (let [x (+ ox (* w i))
                y (+ oy **table-box-size**.y h)]
            (match i
              0 (self.draw-text-box x y sz "1 to 18" DARKGREEN)
              1 (self.draw-text-box x y sz "Even" DARKGREEN)
              2 (self.draw-box x y sz RED)
              3 (self.draw-box x y sz BLACK)
              4 (self.draw-text-box x y sz "Odd" DARKGREEN)
              5 (self.draw-text-box x y sz "19 to 36" DARKGREEN)))))
      (for [x (range 1 4)]
        (for [[i y] (enumerate (range x (+ x 34) 3))]
          (let [fx (int (+ ox (* **table-box-size**.x i)))]
            (self.draw-text-box fx oy **table-box-size** (str y) (get **wheel-colors** (.index **wheel-numbers** y)))))
        (setv oy (- oy (int **table-box-size**.y)))))
    (end-mode-2d)))