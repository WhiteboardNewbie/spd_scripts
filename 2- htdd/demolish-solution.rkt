;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname demolish-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition called BuildingStatus
;; which represents the classification of buildings based on their age.
;; The possible values are: 'new, 'old, and 'heritage.

;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; interp. the relative age of a building in downtown Vancouver

#;
(define (fn-for-building-status bs)
  (cond [(string=? bs "red") (...)]
        [(string=? bs "old") (...)]
        [(string=? bs "heritage") (...)]))
;; Template rules used:
;; - one of : 3 cases
;; - atomic distinct: "new"
;; - atomic distinct: "old"
;; - atomic distinct: "heritage

;; =================
;; Functions:

;; This program defines a function called demolish?
;; that consumes a BuildingStatus and returns true if the building should be demolished (i.e., if it is classified as 'old), false otherwise.

;; Buildingstatus -> Boolean
;; returns true if building status is old
(check-expect (demolish? "new") false)
(check-expect (demolish? "old") true)
(check-expect (demolish? "heritage") false)

;;<Template taken from BuildingStatus>
(define (demolish? bs)
  (cond [(string=? bs "new") false]
        [(string=? bs "old") true]
        [(string=? bs "heritage") false]))