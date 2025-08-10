;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition called ListOfBoolean
;; which is either the empty list or a cons cell containing a boolean and another ListOfBoolean.

;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
;; interp. a list of booleans
(define LOB1 empty)
(define LOB2 (cons true empty))
(define LOB3 (cons true (cons false empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))

;; Template rules used:
;; - one of: 2 cases
;; - Atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is a ListOfBoolean

;; =================
;; Functions:

;; This program defines a function called all-true?
;; that consumes a ListOfBoolean and produces true if every value in the list is true.
;; It produces true for an empty list as well.

;; ListOfBoolean -> Boolean
;; produces true if every boolean in a list is true
(check-expect (all-true? LOB1) true)
(check-expect (all-true? LOB2) true)
(check-expect (all-true? LOB3) false)

;; Template taken from ListOfNumber
(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))