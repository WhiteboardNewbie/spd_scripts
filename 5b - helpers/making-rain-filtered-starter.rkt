;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname making-rain-filtered-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; This program defines an interactive animation of rain drops falling down the screen.
;; The world state is a list of raindrops, each with its own position (x, y).
;; Clicking the mouse creates a new raindrop at the click location (x, y=top).
;; On each tick, every raindrop moves downward by a fixed amount.
;; Raindrops that reach the bottom of the screen are removed from the world state.
;; Helper functions are used to update positions, filter out drops that have fallen off,
;; and compose behaviors clearly following the design rules.

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if me is "button-up" add a new drop at that position
(check-expect (handle-mouse LOD1 50 100 "button-up") (cons (make-drop 50
                                                                      100)
                                                           LOD1))

(define (handle-mouse lod x y me)
  (cond [(mouse=? me "button-up") (cons (make-drop x
                                                   y)
                                        lod)]
        [else
         lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops (cons (make-drop 10 500)
                                (cons (make-drop 20 250)
                                      empty)))
              (cons (make-drop 20
                               (+ SPEED 250))
                    empty))

(define (next-drops lod)
  (filter-drops (increment-y lod)))

;; ListOfDrop -> ListOfDrops
;; filters any drop that has y coordinate greater than HEIGHT
(check-expect (filter-drops (cons (make-drop 10
                                             505)
                                  empty))
              empty)
(check-expect (filter-drops LOD1) LOD1)
(check-expect (filter-drops LOD2) LOD2)
(check-expect (filter-drops (cons (make-drop 10
                                             20)
                                  (cons (make-drop 10
                                                   505)
                                        (cons (make-drop 10
                                                         5)
                                              empty))))
              (cons (make-drop 10
                               20)
                    (cons (make-drop 10
                                     5)
                          empty)))

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (> (drop-y (first lod)) HEIGHT)
             (rest lod)
             (cons (first lod)
                   (filter-drops (rest lod))))]))

;; ListOfDrop -> ListOfDrop
;; increment the y coordinate of the drop by SPEED
(check-expect (increment-y LOD1) LOD1)
(check-expect (increment-y LOD2) (cons (make-drop 10
                                                  (+ SPEED 20)
                                                  )
                                       (cons (make-drop 3
                                                        (+ SPEED 6))
                                             empty)))

(define (increment-y lod)
  (cond [(empty? lod) empty]
        [else
         (cons (make-drop (drop-x (first lod))
                          (+ SPEED (drop-y (first lod))))
               (increment-y (rest lod)))]))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops LOD1) MTS)
(check-expect (render-drops LOD2)
              (place-image DROP
                           (drop-x (first LOD2))
                           (drop-y (first LOD2))
                           (place-image DROP
                                        (drop-x (first (rest LOD2)))
                                        (drop-y (first (rest LOD2)))
                                        MTS)))

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render-next-drop (first lod)
                           (render-drops (rest lod)))]))

;; Drop Image -> Image
;; Renders the next drop oon an existing image
(check-expect (render-next-drop D1 MTS) (place-image DROP
                                                     10
                                                     30
                                                     MTS))

(define (render-next-drop d im)
  (place-image DROP
               (drop-x d)
               (drop-y d)
               im))