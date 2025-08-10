;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname circle-fractal-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; This program defines a recursive function that produces a fractal image composed of circles.
;; Each circle is surrounded by four smaller circles, each two-fifths the size of the parent.
;; The recursion terminates based on a minimum size or depth limit.
;; The function builds one "leaf" shape using beside and above functions,
;; then rotates that shape to create the full surrounding pattern.
;; This approach simplifies the layout and recursive structure of the fractal.

;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

(define (draw-leaf r)
  (if (<= r TRIVIAL-SIZE)
      (circle r "solid" "blue")
      (local [(define center (circle r "solid" "blue"))
              (define leaf (draw-leaf (* r STEP)))]
        (above leaf
               (beside (rotate 90 leaf) center (rotate -90 leaf))))))

(define (circle-fractal r)
  (local [(define leaf (draw-leaf (* r STEP)))]
    (above leaf
           (beside (rotate 90 leaf)
                   (circle r "solid" "blue")
                   (rotate -90 leaf))
           (rotate 180 leaf))))
