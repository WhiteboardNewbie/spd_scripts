;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname movie-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; =================
;; Data definitions:

;; This program defines a data definition called Movie
;; which represents a movie with the following fields:
;; - title: a string
;; - budget: a number representing the movie's budget in dollars
;; - year-released: a number representing the year the movie was released.

(define-struct movie (title budget released))
;; Movie is (make-movie String Number Number)
;; interp. the name, budged and release year of a movie
(define MOVIE-1 (make-movie "Titanic" 200000000 1997))
(define MOVIE-2 (make-movie "Avatar" 237000000 2009))
(define MOVIE-3 (make-movie "The Avengers" 220000000 2012))

#;
(define (fn-for-movie m)
  (... (movie-name m)         ;String
       (movie-budget m)       ;Number
       (movie-released m)))   ;Number

;; Template rules used:
;; - compound: 3 fields

;; =================
;; Functions:

;; This program defines a function called most-recent-movie-title
;; that consumes two Movie data values
;; and produces the title (string) of the movie released most recently (higher year).
;; The function template includes selectors for both movie parameters.

;; Movie Movie-> String
;; consumes two movies and produces the title of the more recently released one
(check-expect (more-recent-movie MOVIE-1 MOVIE-2) "Avatar")
(check-expect (more-recent-movie MOVIE-3 MOVIE-2) "The Avengers")
(check-expect (more-recent-movie MOVIE-1 MOVIE-1) "Titanic")


;; <Template taken from Movie>
(define (more-recent-movie m1 m2)
  (if (>= (movie-released m1) (movie-released m2))
      (movie-title m1)
      (movie-title m2)))