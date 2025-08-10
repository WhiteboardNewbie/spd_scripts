;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition called Student
;; which represents a student with the following fields:
;; - name: a string
;; - grade: a number between 1 and 12
;; - has-allergies: a boolean indicating if the student has allergies (true) or not (false).

(define-struct student (name grade allergies?))
;; Student is (make-student String Natural[1, 12] Boolean)
;; interp. a student with associated name, grade and allergy information
(define STUDENT-1 (make-student"Barry" 6 true))
(define STUDENT-2 (make-student "Amanda" 12 false))
(define STUDENT-3 (make-student "Jake" 5 false))
(define STUDENT-4 (make-student "Misty" 7 true))

#;
(define (fn-for-student s)
  (... (student-name s)          ;String
       (student-grade s)         ;Natural[1, 12]
       (student-allergies? s)))  ;Boolean

;; =================
;; Functions:

;; This program defines a function called add-to-allergy-list?
;; that consumes a Student and produces true if the student is in grade 6 or below
;; and has allergies, false otherwise.

;; Student -> Boolean
;; produces true if a student has allergies and is in grade 6 or below
(check-expect (add-student-to-special-list? STUDENT-1) true)
(check-expect (add-student-to-special-list? STUDENT-2) false)
(check-expect (add-student-to-special-list? STUDENT-3) false)
(check-expect (add-student-to-special-list? STUDENT-4) false)

;; <Template taken from Student>
(define (add-student-to-special-list? s)
  (and (<= (student-grade s) 6)         ;Natural[1, 12]
       (student-allergies? s)))  ;Boolean