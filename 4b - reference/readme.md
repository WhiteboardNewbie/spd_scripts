## Spinning Bears
The task was to produce an animation featuring a spinning bear, simple enough!. The fun part was adding the functionality to produce another spinning bear wherever clicked. It was interesting to note that lists (of bears!) could be used to this effect, i.e, clicking anywhere adds another bear to the list.
```
(require 2htdp/image)
(require 2htdp/universe)

;; This program defines a world animation of spinning bears.
;; Initially, the world contains one bear positioned at the center of the screen, spinning counterclockwise.
;; Each bear has compound data consisting of:
;; - x: Number (horizontal position)
;; - y: Number (vertical position)
;; - angle: Number (current rotation angle in degrees)
;;
;; Clicking the mouse replaces the single bear with a new one at the clicked position, starting upright.
;; The program can be expanded to handle an arbitrary number of bears, each with their own position and rotation angle, spinning independently.

;; Spinning Bears

;; =================
;; Constants:

(define SCREEN-HEIGHT 500)
(define SCREEN-WIDTH 500)
(define CTR-Y (/ SCREEN-HEIGHT 2))
(define CTR-X (/ SCREEN-WIDTH 2))
(define MTS (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))


(define BEAR .)

(define ROTARY-SPEED 5)

;; =================
;; Data definitions:

(define-struct bear (x y th))
;; Bear is (make-bear Number[0 WIDTH] Number[0 HEIGHT] Number[0, 360))
;; interp. a bear with x and y positions and angle of rotation th
(define BEAR1 (make-bear 0 0 0))
(define BEAR2 (make-bear 50 100 60))
(define BEAR3 (make-bear 100 50 90))
(define BEAR4 (make-bear 100 100 270))

(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-th b)))

;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
(define LOB1 empty)
(define LOB2 (cons BEAR2 empty))
(define LOB3 (cons BEAR3 (cons BEAR2 empty)))

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bear (first lob))
              (fn-for-lob (rest lob)))]))
;; =================
;; Functions:

;; ListOfBear -> ListOfBear
;; start the world with (main (cons (make-bear CTR-X CTR-Y 0) empty))
;;
(define (main lob)
  (big-bang lob                  ; ListOfBear
    (on-tick   rotate-bears)     ; ListOfBear -> ListOfBear
    (to-draw   render-bears)     ; ListOfBear -> Image
    (on-mouse  handle-mouse)))   ; ListOfBear Integer Integer MouseEvent -> ListOfBear


;; ListOfBear -> ListOfBear
;; rotate the bears in the list by ROTARY-SPEED degrees
(check-expect (rotate-bears LOB1) empty)
(check-expect (rotate-bears LOB2) (cons (make-bear 50
                                                   100
                                                   (+ ROTARY-SPEED 60))
                                        empty))
(check-expect (rotate-bears LOB3) (cons (make-bear 100
                                                   50
                                                   (+ ROTARY-SPEED 90))
                                        (cons (make-bear 50
                                                         100
                                                         (+ ROTARY-SPEED 60))
                                              empty)))

;; Template taken from ListOfBear
(define (rotate-bears lob)
  (cond [(empty? lob) empty]
        [else
         (cons (rotate-bear (first lob))
               (rotate-bears (rest lob)))]))

;; Bear -> Bear
;; rotates a bear by ROTARY-SPEED degrees, helper for rotate-bears
(check-expect (rotate-bear BEAR2) (make-bear 50
                                             100
                                             (+ ROTARY-SPEED 60)))

;; Template taken from Bear
(define (rotate-bear b)
  (make-bear (bear-x b)
             (bear-y b)
             (modulo (+ ROTARY-SPEED (bear-th b)) 360)))


;; ListOfBear -> Image
;; render the Bears in the list 
(check-expect (render-bears LOB1) MTS)
(check-expect (render-bears LOB2)
              (place-image (rotate (bear-th (first LOB2))
                                   BEAR)
                           (bear-x (first LOB2))
                           (bear-y (first LOB2))
                           MTS))
(check-expect (render-bears LOB3)
              (place-image (rotate 90
                                   BEAR)
                           100
                           50
                           (place-image (rotate 60
                                                BEAR)
                                        50
                                        100
                                        MTS)))

;; Template taken from ListOfBear
(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else
         (render-bear (first lob) (render-bears (rest lob)))]))

;; Bear Image -> Image
;; Places the next bear on an existing scene, helper for render-bears
(check-expect (render-bear BEAR2 MTS) (place-image (rotate (bear-th BEAR2)
                                                           BEAR)
                                                   (bear-x BEAR2)
                                                   (bear-y BEAR2)
                                                   MTS))
;; Template taken from Bear
(define (render-bear b scene)
  (place-image (rotate (bear-th b)
                       BEAR)
               (bear-x b)
               (bear-y b)
               scene))


;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; create a new bear at position x and y with rotation angle 0 when position x, y is clicked on the screen
(check-expect (handle-mouse LOB1 300 350 "button-up")
              (cons (make-bear 300 350 0) empty))
(check-expect (handle-mouse LOB2 300 350 "button-up")
              (cons (make-bear 300 350 0) LOB2))

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-up") (cons (make-bear x y 0) lob)]
        [else lob]))
```
![Spinning Bears.](/assets//gifs//spinning-bears.gif)