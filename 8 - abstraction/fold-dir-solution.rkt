;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fold-dir-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; This program defines an abstract fold function called fold-dir
;; that consumes a Dir (directory structure) and functions to process images and subdirectories,
;; and combines these to produce a result.

;; Using fold-dir, it defines:
;; - a function to count all images in a Dir and its subdirectories,
;; - a function to search for a directory by name in a Dir and its subdirectories, producing true or false.

;; Regarding part D: fold-dir is great for aggregations like counting images,
;; but for searching (part C), fold-dir may be less efficient since it always traverses the whole tree,
;; whereas a direct recursive search can short-circuit when a matching directory is found.
;; Therefore, fold-dir might not be the best for search tasks requiring early termination.

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:

#;
(define (dir-fn dir)
  (local [(define (fn-for-dir dir)
            (... (dir-name dir)
                 (fn-for-loi (dir-images dir))
                 (fn-for-lod (dir-sub-dirs dir))))
          (define (fn-for-lod lod)
            (cond [(empty? lod) (...)]
                  [else
                   (... (fn-for-dir (first lod))
                        (fn-for-lod (rest lod)))]))
          (define (fn-for-loi loi)
            (cond [(empty? loi) (...)]
                  [else
                   (... (first lod)
                        (fn-for-loi (rest loi)))]))]
    (fn-for-dir dir)))



;; (String X X -> X) ( X X -> X) (Image X -> X) X X Dir -> X
;; fold function for Dir
(define (fold-dir c1 c2 c3 i1 i2 dir)
  (local [(define (fn-for-dir dir)
            (c1  (dir-name dir)
                 (fn-for-loi (dir-images dir))
                 (fn-for-lod (dir-sub-dirs dir))))
          (define (fn-for-lod lod)
            (cond [(empty? lod) i1]
                  [else
                   (c2 (fn-for-dir (first lod))
                        (fn-for-lod (rest lod)))]))
          (define (fn-for-loi loi)
            (cond [(empty? loi) i2]
                  [else
                   (c3 (first loi)
                       (fn-for-loi (rest loi)))]))]
    (fn-for-dir dir)))




;; Dir -> Number
;; Consumes a Dir and produces the number of images in the directory and it's sub-directories
(check-expect (dir-image-count D4) 2)
(check-expect (dir-image-count D5) 1)
(check-expect (dir-image-count D6) 3)
(define (dir-image-count dir)
  (local [(define (c1 name image-count sub-dir-count)
            (+ image-count sub-dir-count))
          (define (c2 first-count rest-count)
            (+ first-count rest-count))
          (define (c3 first rest-count)
            (+ 1 rest-count))]
    (fold-dir c1 c2 c3 0 0 dir)))



;; Dir String -> Boolean
;; Consumes a Dir and a name and produces true if a sub-directory with the name exists, otherwise false
(check-expect (sub-dir-exists? D4 "D4") true)
(check-expect (sub-dir-exists? D5 "D4") false)
(check-expect (sub-dir-exists? D5 "D6") false)
(check-expect (sub-dir-exists? D6 "D4") true)
(define (sub-dir-exists? dir name)
  (local [(define (c1 dir-name images-dummy-value sub-dirs-search-value)
            (or (string=? dir-name name) sub-dirs-search-value))
          (define (c2 first-sub-dir-search-value rest-search-value)
            (or first-sub-dir-search-value rest-search-value))
          (define (c3 first-image rest-images)
            rest-images)]
    (fold-dir c1 c2 c3 false false dir)))



;; not the best way because it forces traversal through the entire tree instead of stopping search when a value is found
