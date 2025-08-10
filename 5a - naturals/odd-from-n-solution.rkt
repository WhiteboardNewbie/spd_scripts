;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname odd-from-n-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; This program defines a function called odd-from-n
;; that consumes a natural number n and produces a list of all odd numbers
;; from n down to 1 (in descending order).
;; It uses the primitive odd? to check if a number is odd.

;; Natural -> ListOfNatural
;; consumes a natural n and produces the list of naturals from n to 1 (inclusive)
(check-expect (odd-list 5) (cons 5 (cons 3 (cons 1 empty))))
(check-expect (odd-list 0) empty)
(check-expect (odd-list 2) (cons 1 empty))
(check-expect (odd-list 1) (cons 1 empty))

(define (odd-list n)
  (cond [(zero? n) empty]
        [else
         (if (odd? n)
             (cons n (odd-list (sub1 n)))
             (odd-list (sub1 n)))]))