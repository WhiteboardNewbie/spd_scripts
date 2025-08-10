;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname double-all-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition for a list of numbers
;; which is either the empty list or a cons cell containing a number and another list of numbers.

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

;; This program defines a function called double-all
;; that consumes a list of numbers and produces a new list
;; where every number in the original list is doubled.

;; ListOfNumber -> ListOfNumber
;; doubles every number in a ListOfNumber
(check-expect (double-all empty) empty)
(check-expect (double-all (cons 1.5 empty)) (cons 3 empty))
(check-expect (double-all (cons 2 (cons 6.5 empty))) (cons 4 (cons 13 empty)))

;; Template taken from ListOfNumber
(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* (first lon) 2)
               (double-all (rest lon)))]))