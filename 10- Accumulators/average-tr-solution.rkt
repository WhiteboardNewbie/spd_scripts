;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname average-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function called average
;; that consumes a list of numbers and produces the average (mean) of those numbers.

;; listof Number -> Number
;; consumes a listof Number and produces the average of the numbers in the list
(check-expect (average empty) 0)
(check-expect (average (list 1 2 3 4 5)) 3)

(define (average lon0)
  (cond [(empty? lon0) 0]
        [else
         (local [(define (average lon rsf acc)
                   (cond [(empty? lon) (/ rsf acc)]
                         [else
                          (average (rest lon) (+ rsf (first lon)) (add1 acc))]))]
                   (average lon0 0 0))]))