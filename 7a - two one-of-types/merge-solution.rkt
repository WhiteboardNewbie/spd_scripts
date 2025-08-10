;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function called merge
;; that consumes two lists of numbers sorted in ascending order
;; and produces a single list containing all numbers from both lists,
;; sorted in ascending order.
;; The function uses a cond with three cases and a cross product type-comment table
;; to handle all combinations of empty and non-empty input lists.

;; ListOfNumber ListOfNumber -> ListOfNumber
;; ASSUME: both input lists already sorted in ascending order
;; Merges two sorted lists into one, in ascending order
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge empty (list 3 4)) (list 3 4))
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge (list 3 4) empty) (list 3 4))
(check-expect (merge (list 1) (list 3)) (list 1 3))
(check-expect (merge (list 3) (list 4)) (list 3 4))
(check-expect (merge (list 3) (list 3)) (list 3 3))
(check-expect (merge (list 5 6) (list 4 5)) (list 4 5 5 6))
(check-expect (merge (list 4 5) (list 5 6)) (list 4 5 5 6))
(check-expect (merge (list 1) (list 3 4)) (list 1 3 4))
(check-expect (merge (list 1 2 3 4) (list 3 4)) (list 1 2 3 3 4 4))

#;
(define (merge lsta lstb)
  (cond [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else
         (if (<= (first lsta) (first lstb))
             (cons (first lsta) (merge (rest lsta) lstb))
             (cons (first lstb) (merge lsta (rest lstb))))]))

(define (merge lsta lstb)
  (cond [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else
         (if (<= (first lsta) (first lstb))
             (append (list (first lsta)) (merge (rest lsta) lstb))
             (append (list (first lstb)) (merge lsta (rest lstb))))]))