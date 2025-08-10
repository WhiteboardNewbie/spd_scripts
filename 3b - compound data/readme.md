## Water Balloon
The program makes an animation of a rotating water balloon. Pressing Space resets the animation.
![The water balloon image used.](/assets/images/water-balloon.png)
```
(require 2htdp/image)
(require 2htdp/universe)

;; Water Balloon Simulation

;; =================
;; Constants:
(define WATER-BALLOON )
(define HEIGHT 200)
(define WIDTH 500)
(define MTS (empty-scene WIDTH HEIGHT))
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define SPEED 5)
(define ROTARY-SPEED 5)
;; =================
;; Data definitions:

(define-struct balloon (x th))
;; Balloon is (make-balloon Number[0, WIDTH] Number[0, 360)
;; interp. x is the x-position and th the angle of rotation of a balloon 
(define BALLOON1 (make-balloon 0 0))
(define BALLOON2 (make-balloon WIDTH 359))
(define BALLOON3 (make-balloon (/ WIDTH 2) 45))

#;
(define (fn-for-balloon b)
  (... (balloon-x b)
       (balloon-th b)))

;; Template rules used:
;; - compound: 2 fields

;; =================
;; Functions:

;; Balloon -> Balloon
;; start the world with (main BALLOON1)
;; 
(define (main b)
  (big-bang b                        ; Balloon
    (on-tick   get-next-balloon)     ; Balloon -> Balloon
    (to-draw   render-balloon)       ; Balloon -> Image
    (on-key    handle-key)))         ; Balloon KeyEvent -> Balloon

;; Balloon -> Balloon
;; Add SPEED to x-position and rotate the balloon by ROTARY-SPEED degree clockwise
(check-expect (get-next-balloon BALLOON1) (make-balloon (+ (balloon-x BALLOON1) SPEED) (+ ROTARY-SPEED (balloon-th BALLOON1))))
(check-expect (get-next-balloon BALLOON2) (make-balloon (+ WIDTH SPEED) (modulo (+ ROTARY-SPEED (balloon-th BALLOON2)) 360))) 

;; Template taken from Balloon
(define (get-next-balloon b)
  (make-balloon (+ (balloon-x b) SPEED)
                (modulo (+ (balloon-th b) ROTARY-SPEED) 360)))

;; Balloon -> Image
;; render the current balloon state 
(check-expect (render-balloon BALLOON1) (place-image (rotate (balloon-th BALLOON1) WATER-BALLOON) (balloon-x BALLOON1) CTR-Y MTS))

;; Template taken from Balloon
(define (render-balloon b)
  (place-image (rotate (balloon-th b) WATER-BALLOON)
               (balloon-x b)
               CTR-Y
               MTS))

;; Balloon -> Balloon
;; when ke is " ", reset balloon-x to 0 and balloon-th to the initial value
(check-expect (handle-key BALLOON1 " ") BALLOON1)
(check-expect (handle-key BALLOON1 "a") BALLOON1)
(check-expect (handle-key BALLOON2 " ") BALLOON1)
(check-expect (handle-key BALLOON2 "a") BALLOON2)

(define (handle-key b ke)
  (cond [(key=? ke " ") BALLOON1]
        [else 
         b]))

```