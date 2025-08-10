;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define RECTANGLE (rectangle 100 150 "outline" "black"))
(define TRIANGLE (triangle 5 "solid" "red"))
(define CIRCLE (circle 10 "solid" "blue"))
;; =================
;; Data definitions:

;; This program defines a data definition called ListOfImage
;; which is either the empty list or a cons cell containing an image and another ListOfImage.

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images
(define LOI1 empty)
(define LOI2 (cons RECTANGLE empty))
(define LOI3 (cons TRIANGLE (cons CIRCLE empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - Atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is a ListOfImage

;; =================
;; Functions:

;; This program defines a function called sum-image-areas
;; that consumes a ListOfImage and produces a number representing
;; the total sum of the areas (width × height) of all images in the list.

;; ListOfImage -> Number
;; sums the area of every image in the list, produces 0 if the list is empty
(check-expect (area-sum LOI1) 0)
(check-expect (area-sum LOI2) 15000)
(check-expect (area-sum LOI3) (+ (* (image-height TRIANGLE) (image-width TRIANGLE)) (* (image-height CIRCLE) (image-width CIRCLE))))

;; Template taken from ListOfImage
(define (area-sum loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-height (first loi))
               (image-width (first loi)))
              (area-sum (rest loi)))]))