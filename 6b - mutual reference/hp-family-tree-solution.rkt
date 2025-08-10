;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-starter) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; This program defines a data definition called DescendantTree
;; representing a Harry Potter descendant family tree node, including:
;; - name: String
;; - patronus: String ("" if unknown)
;; - wand-material: String ("" if unknown)
;; - children: ListOf DescendantTree (possibly empty)
;;
;; It supports recursive family trees with arbitrary descendants.

;; It defines a constant ARTHUR representing Arthur Weasley's family tree,
;; including all his children and grandchildren Lily, Victoire, Albus, and James,
;; using "" for unknown information as needed.

;; It defines a function that produces a list of pairs (two-element lists) of all people in a tree
;; with their patronus strings.

;; It defines a function that produces a list of names of people whose wand-material matches a given string.

;; Data Definitons

(define-struct character (name wand patronus descendants))
;; Character is (make-character String String String ListOfCharacter)
;; interp. the name, wand wood type, patronus and descents of a harry potter character. missing information is represented with "" (the empty string)

;; ListOfCharacter is one of:
;; - empty
;; - (cons Character ListOfCharacter)
;; interp. a list of harry potter characters

(define ARTHUR
  (make-character "Arthur" "" "Weasel"
                  (list (make-character "Bill" "" "" (list (make-character "Victoire"  "" "" empty)      
                                                           (make-character "Dominique" "" "" empty)   
                                                           (make-character "Louis"     "" "" empty)))   
                        (make-character "Charlie" "ash" "" empty)
                        (make-character "Percy" "" "" (list (make-character "Molly" "" "" empty)          
                                                            (make-character "Lucy"  "" "" empty)))
                        (make-character "Fred"    ""    "" empty)
                        (make-character "George"  ""    "" (list (make-character "Fred" "" "" empty)     
                                                                 (make-character "Roxanne"  "" "" empty)))
                        (make-character "Ron"     "ash" "Jack Russell Terrier" (list (make-character "Rose" "" "" empty)  
                                                                                     (make-character "Hugo" "" "" empty)))
                        (make-character "Ginny"   ""    "Horse" 
                                        (list (make-character "James" "" "" empty)
                                              (make-character "Albus" "" "" empty)
                                              (make-character "Lily"  "" "" empty))))))

#;
(define (fn-for-character c)
  (... (character-name c)
       (character-wand c)
       (character-patronus c)
       (fn-for-loc (character-descendants c))))

#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-character (first loc))
              (fn-for-loc (rest loc)))]))

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (list "Harry" "Stag"))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; ListOfPair is one of:
;; - empty
;; - (cons (list String String) ListOfPair)
;; interp. a list of 2 element lists

(define LOP1 empty)
(define LOP2 (list (list "Harry" "Stag") (list "Hermione" "Otter")))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-los (first lop))
              (fn-for-lop (rest lop)))]))

;; Functions

;; Character       -> ListOfPairs
;; ListOfCharacter -> ListOfPair
;; interp. produce a list, containing lists of character name and patronus
(check-expect (descendant-patroni empty) empty)
(check-expect (character-patroni (make-character "a" "b" "c" empty)) (list (list "a" "c")))
(check-expect (character-patroni ARTHUR) (list (list "Arthur" "Weasel")
                                               (list "Bill" "")
                                               (list "Victoire" "")
                                               (list "Dominique" "")
                                               (list "Louis" "")
                                               (list "Charlie" "")
                                               (list "Percy" "")
                                               (list "Molly" "")
                                               (list "Lucy" "")
                                               (list "Fred" "")
                                               (list "George" "")
                                               (list "Fred" "")
                                               (list "Roxanne" "")
                                               (list "Ron" "Jack Russell Terrier")
                                               (list "Rose" "")
                                               (list "Hugo" "")
                                               (list "Ginny" "Horse")
                                               (list "James" "")
                                               (list "Albus" "")
                                               (list "Lily" "")))

;; <Templates taken from Character and ListOfCharacter>

(define (character-patroni c)
  (cons (list (character-name c)
              (character-patronus c))
        (descendant-patroni (character-descendants c))))

(define (descendant-patroni loc)
  (cond [(empty? loc) empty]
        [else
         (append (character-patroni (first loc))
                 (descendant-patroni (rest loc)))]))

;; Character String       -> ListOfString
;; ListOfCharacter String -> ListOfString
;; produce the name of every person from a tree of a character and his descendants that have a wand of a given material
(check-expect (wand-match-loc empty "ash") empty)
(check-expect (wand-match-character (make-character "a" "b" "c" empty) "ash") empty)
(check-expect (wand-match-character (make-character "a" "ash" "c" empty) "ash") (list "a"))
(check-expect (wand-match-character ARTHUR "ash") (list "Charlie" "Ron"))

;; <Templates taken from Character and ListOfCharacter with atomic parameter w>

(define (wand-match-character c w)
  (if  (string=? (character-wand c) w)
       (cons (character-name c)
             (wand-match-loc (character-descendants c) w))
       (wand-match-loc (character-descendants c) w)))

(define (wand-match-loc loc w)
  (cond [(empty? loc) empty]
        [else
         (append (wand-match-character (first loc) w)
                 (wand-match-loc (rest loc) w))]))








