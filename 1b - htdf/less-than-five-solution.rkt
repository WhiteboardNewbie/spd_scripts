;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname less-than-five-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function that takes a string
;; and returns true if its length is less than 5.

;; String -> Boolean
;; returns true if length of string is less than 5

;(define (lt-five n) true) ;stub

(check-expect (lt-five 5) false)
(check-expect (lt-five 6.7) false)
(check-expect (lt-five 2.3) true)
(check-expect (lt-five 4) (< 4 5))

;(define (lt-five n) ;template
;  (... n))

(define (lt-five n)
  (< n 5))