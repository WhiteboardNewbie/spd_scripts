;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


;; This program defines a function called decreasing-image
;; that consumes a natural number n and produces an image displaying
;; the numbers from n down to 0 side by side.

;; Natural -> Image
;; Produces an image of all naturals from n to 0 side by side

(define (decreasing-image n)
  (cond [(zero? n) (text (number->string n) 24 "black")]
        [else
         (beside/align "center"
                       (text (number->string n) 24 "black")
                       (decreasing-image (sub1 n)))]))
