;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; This program defines an animation of a countdown starting at 10,
;; which decreases by one each clock tick until it reaches zero,
;; then stays at zero.
;; The countdown updates once every second.
;; Additionally, when the spacebar is pressed, the countdown resets to 10.

(require 2htdp/image)
(require 2htdp/universe)

;; My world program  (make this more specific)

;; =================
;; Constants:
(define TEXT-SIZE 24)
(define TEXT-COLOUR "black")
(define BOX-COLOUR "black")
(define BOX-HEIGHT 100)
(define BOX-WIDTH 100)
(define CTR-Y (/ BOX-HEIGHT 2))
(define CTR-X (/ BOX-WIDTH 2))
(define BOX (rectangle BOX-HEIGHT BOX-WIDTH "outline" BOX-COLOUR))

;; =================
;; Data definitions:

;; CountDown is Number[0, 10]
;; interp. The number displayed on the countdown
(define CD1 0)
(define CD2 10)
(define CD3 4)

#;
(define (fn-for-count-down cd)
  (... cd))

;; Template rules used:
;; - Atomic non-distinct: Number[0, 10]

;; =================
;; Functions:

;; CountDown -> CountDown
;; start the world with (main 10)
;; 
(define (main cd)
  (big-bang cd                   ; CountDown
    (on-tick   advance-cd 1)     ; CountDown -> CountDown
    (to-draw   render-cd)   ; CountDown -> Image
    (on-key    handle-key)))    ; CountDown KeyEvent -> CountDown

;; CountDown -> CountDown
;; Decrease CountDown by 1
(check-expect (advance-cd 0) 0)
(check-expect (advance-cd 5) 4)

;; <Template taken from CountDown>
(define (advance-cd cd)
  (if (= cd 0)
      0
      (- cd 1)))

;; CountDown -> Image
;; render the CountDown 
(check-expect (render-cd 0) (overlay BOX (text (number->string 0) TEXT-SIZE TEXT-COLOUR)))
(check-expect (render-cd 6) (overlay BOX (text (number->string 6) TEXT-SIZE TEXT-COLOUR)))

;; <Template taken from CountDown>
(define (render-cd cd)
  (overlay BOX (text (number->string cd) TEXT-SIZE TEXT-COLOUR)))

;; CountDown KeyEvent -> CountDown
;; reset the countdown when ke is " "
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 10 "a") 10)
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 0 "a") 0)

(define (handle-key cd ke)
  (cond [(key=? ke " ") 10]
        [else 
         cd]))

(main 10)