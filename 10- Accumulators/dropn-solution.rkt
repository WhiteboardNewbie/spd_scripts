;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function called dropn
;; that consumes a list of elements lox and a natural number n
;; and produces a new list formed by removing every nth element from lox.
;; For example, (dropn (list 1 2 3 4 5 6 7) 2) produces (list 1 2 4 5 7).

;; listof Number Natural -> listofNumber
;; consumes a listof Number and produces the list formed by dropping every nth element from the list
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))

(define (dropn lon0 n)
  (local [(define (dropn lon acc)
            (cond [(empty? lon) empty]
                  [else
                   (if (zero? acc) 
                       (dropn (rest lon) n)
                       (cons (first lon) (dropn (rest lon) (sub1 acc))))]))]
    (dropn lon0 n)))