;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname largest-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition for a list of numbers
;; as either the empty list, or a cons cell with a number and another list of numbers.

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

;; This program defines a function called largest-in-list
;; that consumes a list of numbers and produces the largest number in the list.
;; If the list is empty, it produces 0.
;; Assumes all numbers are greater than 0.

;; ListOfNumber -> Number
;; returns the largest number in a ListOfNumber and 0 if the list is empty
(check-expect (largest LON1) 0)
(check-expect (largest (cons 5 empty)) 5)
(check-expect (largest LON2) 60)

;; Template taken from ListOfNumber
(define (largest lon)
  (cond [(empty? lon) 0]
        [else
         (max (first lon)
              (largest (rest lon)))]))

