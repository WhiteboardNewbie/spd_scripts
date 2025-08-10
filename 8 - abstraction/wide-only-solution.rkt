;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wide-only-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; This program defines a function called wide-only
;; that consumes a list of images and produces a list containing only those images
;; whose width is greater than their height, using the built-in filter function.

;; (listof Image) -> (listof Image)
;; consumes a list of images and produces only those which are wider than they are tall
(check-expect (wide-only empty) empty)
(check-expect (wide-only (list (rectangle 40 20 "outline" "black")
                               (rectangle 20 40 "outline" "black")))
              (list (rectangle 40 20 "outline" "black")))
(define (wide-only loi)
  (local [(define (wide? img)
           (> (image-width img) (image-height img)))]
  (filter wide? loi)))