;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname function-writing-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function that takes two numbers
;; and returns the larger one.

(define (greater_of_two_numbers a b) (if (> a b)
                                         a
                                         b))

(greater_of_two_numbers 2 2)