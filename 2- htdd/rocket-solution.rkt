;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; =================
;; Data definitions:

;; This program defines a data definition called RocketDescent
;; which represents the rocket's remaining distance to touchdown in kilometers.
;; It is a number between 0 and 100, where 0 means the rocket has landed.

;; RocketDescent is one of:
;; - Natural[1, 100]
;; - "done"
;; interp. either the number of kilometers remaining to touchdown or "done", meaning touchdown
(define RD1 1)
(define RD2 100)
(define RD3 53)
(define RD4 "done")

#;
(define (fn-for-rocket-descent rd)
  (cond [(number? rd) (... rd)]
        [else (...)]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic non-distinct: Natural[1, 100]
;; - atomic distinct: "done"
;; =================
;; Functions:

;; This program defines a function called rocket-descent-to-msg
;; that consumes a RocketDescent (a number between 0 and 100)
;; and produces a short string message:
;; - If the descent is 0, returns "The rocket has landed!"
;; - Otherwise, returns a string indicating the remaining kilometers to touchdown.

;; RocketDescent -> Image
;; prints the rocket's remaining descent distance, when the descent is over prints "The rocket has landed!"
(check-expect (rocket-descent-to-msg 100) (text (number->string 100)
                                                24
                                                "white"))
(check-expect (rocket-descent-to-msg 53) (text (number->string 53)
                                               24
                                               "white"))
(check-expect (rocket-descent-to-msg 1) (text (number->string 1)
                                              24
                                              "white"))
(check-expect (rocket-descent-to-msg "done") (text "The rocket has landed!"
                                                   24
                                                   "white"))

;<Template taken from RocketDescent>

(define (rocket-descent-to-msg rd)
  (cond [(number? rd) (text (number->string rd)
                            24
                            "white")]
        [else (text "The rocket has landed!"
                    24
                    "white")]))

(rocket-descent-to-msg "done")