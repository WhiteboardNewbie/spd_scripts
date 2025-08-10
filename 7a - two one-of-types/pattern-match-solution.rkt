;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pattern-match-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function that consumes a sequence of characters (a string)
;; and determines whether its beginning matches a given pattern string.
;; The matching is limited to checking if the first part of the input equals the pattern exactly.
;; also defines a function called pattern-match?
;; that consumes two parameters:
;; - Pattern: a non-empty list of strings representing the pattern to match
;; - ListOf1String: a non-empty list of strings to be checked against the pattern
;; The function produces true if the pattern matches the beginning of ListOf1String.
;; If ListOf1String is longer than Pattern but starts with Pattern, returns true.
;; If ListOf1String is shorter than Pattern, returns false.
;;
;; The design uses a cross product of type comments table to reduce the cond cases to 4,
;; simplifying the matching logic accordingly.
;; It employs helper functions to compare head elements and recurse on the tails.

;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

;; ==========
;; Functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))

;; ListOflString Pattern -> Boolean
;; produces true if pattern matches the ListOflString
(check-expect (pattern-match? empty empty) true)
(check-expect (pattern-match? empty (list "A")) false)
(check-expect (pattern-match? empty (list "N")) false)
(check-expect (pattern-match? (list "y") empty) true)
(check-expect (pattern-match? (list "6") empty) true)
(check-expect (pattern-match? (list "y") (list "A")) true)
(check-expect (pattern-match? (list "6") (list "A")) false)
(check-expect (pattern-match? (list "y") (list "N")) false)
(check-expect (pattern-match? (list "6") (list "N")) true)
(check-expect (pattern-match? (list "y") (list "A" "N")) false)
(check-expect (pattern-match? (list "y" "6") (list "A" "N")) true)
(check-expect (pattern-match? (list "y" "y") (list "A" "N")) false)
(check-expect (pattern-match? (list "y" "6" "a") (list "A" "N")) true)
(check-expect (pattern-match? (list "y" "6" "a") (list "A" "A")) false)

(define (pattern-match? los pattern)
  (cond [(empty? pattern) true]
        [(empty? los) false]
        [(string=? (first pattern) "A") (if (alphabetic? (first los))
                                            (pattern-match? (rest los) (rest pattern))
                                            false)]
        [else
         (if (numeric? (first los))
             (pattern-match? (rest los) (rest pattern))
             false)]))
