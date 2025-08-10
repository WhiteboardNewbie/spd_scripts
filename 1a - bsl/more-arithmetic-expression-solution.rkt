;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname more-arithmetic-expression-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; This program demonstrates two ways to multiply the numbers 3, 5, and 7:
;; 1) Using the * operator with multiple arguments.
;; 2) Multiplying step-by-step by reusing intermediate results.

(* 3 5 7)
(* (* 3 5) 7)