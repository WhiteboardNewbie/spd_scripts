;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname cantor-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; This program defines a function called cantor that consumes a width w
;; and produces a Cantor set fractal image of that width by recursively drawing bars separated by whitespace.

;; It extends the function to accept a second parameter controlling the percentage of whitespace between bars,
;; allowing flexible spacing (e.g., 1/3 reproduces the classic Cantor set).

;; It defines a world animation where the world state is the latest x-coordinate of the mouse,
;; and the draw handler calls the cantor function using the world state as width
;; and the mouse's x-position divided by the width as the whitespace percentage parameter,
;; creating an interactive Cantor set visualization.

(define CUTOFF 8)
(define LINE-HEIGHT 10)
(define SPACE-HEIGHT 5)
;; Number -> Image
;; Consumes a width and produces a cantor set of the given width
(check-expect (cantor CUTOFF) (rectangle CUTOFF LINE-HEIGHT "solid" "blue"))
(check-expect (cantor 63)
              (above (rectangle 63 LINE-HEIGHT "solid" "blue")
                     (rectangle 63 SPACE-HEIGHT "solid" "white")
                     (beside (above (rectangle 21 LINE-HEIGHT "solid" "blue")
                                    (rectangle 21 SPACE-HEIGHT "solid" "white")
                                    (beside (rectangle 7 LINE-HEIGHT "solid" "blue")
                                            (rectangle 7 LINE-HEIGHT "solid" "white")                                       
                                            (rectangle 7 LINE-HEIGHT "solid" "blue")))
                             (rectangle 21 LINE-HEIGHT "solid" "white")
                             (above (rectangle 21 LINE-HEIGHT "solid" "blue")
                                    (rectangle 21 SPACE-HEIGHT "solid" "white")
                                    (beside (rectangle 7 LINE-HEIGHT "solid" "blue")
                                            (rectangle 7 LINE-HEIGHT "solid" "white")
                                            (rectangle 7 LINE-HEIGHT "solid" "blue"))))))

(define (cantor w)
  (cond [(<= w CUTOFF) (rectangle w LINE-HEIGHT "solid" "blue")]
        [else
         (above (rectangle w LINE-HEIGHT "solid" "blue")
                (rectangle w SPACE-HEIGHT "solid" "white")
                (local [(define sub (cantor (/ w 3)))
                        (define wht (rectangle (/ w 3) LINE-HEIGHT "solid" "white"))]
                  (beside sub
                          wht
                          sub)))]))

;; Number Number -> Image
;; Same as cantor but the number p controls what percentage of the total width at each recursive step is the white part
;; if p=4 then 1/4th of the width is white and 3/4 of the width is blue
(check-expect (cantor-whiter 63 3)
              (above (rectangle 63 LINE-HEIGHT "solid" "blue")
                     (rectangle 63 SPACE-HEIGHT "solid" "white")
                     (beside (above (rectangle 21 LINE-HEIGHT "solid" "blue")
                                    (rectangle 21 SPACE-HEIGHT "solid" "white")
                                    (beside (rectangle 7 LINE-HEIGHT "solid" "blue")
                                            (rectangle 7 LINE-HEIGHT "solid" "white")                                       
                                            (rectangle 7 LINE-HEIGHT "solid" "blue")))
                             (rectangle 21 LINE-HEIGHT "solid" "white")
                             (above (rectangle 21 LINE-HEIGHT "solid" "blue")
                                    (rectangle 21 SPACE-HEIGHT "solid" "white")
                                    (beside (rectangle 7 LINE-HEIGHT "solid" "blue")
                                            (rectangle 7 LINE-HEIGHT "solid" "white")
                                            (rectangle 7 LINE-HEIGHT "solid" "blue"))))))

(define (cantor-whiter w p)
  (cond [(<= w CUTOFF) (rectangle w LINE-HEIGHT "solid" "blue")]
        [else
         (above (rectangle w LINE-HEIGHT "solid" "blue")
                (rectangle w SPACE-HEIGHT "solid" "white")
                (local [(define white-width (/ w p))
                        (define sub (cantor (/ (- w white-width) 2)))
                        (define wht (rectangle white-width LINE-HEIGHT "solid" "white"))]
                  (beside sub
                          wht
                          sub)))]))

;; Cantor Sets

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)
(define MTS (rectangle WIDTH HEIGHT "outline" "white"))

;; =================
;; Data definitions:

;; X is Number
;; interp. X is the most recent x-coordinate of the mouse
(define x1 10)
(define x2 0)

#;
(define (fn-for-X X)
  (... X))

;; =================
;; Functions:

;; X -> X
;; start the world with x=0
;; 
(define (main X)
  (big-bang X                  ; X
            (to-draw   render)   ; X -> Image
            (on-mouse  handle-mouse)))      ; X Integer Integer MouseEvent -> X

;; X -> Image
;; calls cantor-whiter and renders the cantor set produced onto MTS with width = WIDTH
;; and fraction p equal to the x coordinate of the mouse
;; !!!
(define (render X)
  (overlay/align "middle"
                 "top"
                 (cantor-whiter WIDTH (/ X (* WIDTH 10)))              
                 MTS))

;; X Integer Integer MouseEvent -> X
;; !!!
(define (handle-mouse X x y me)
  (cond [(mouse=? me "move") x]
        [else
         X]))