;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname contains-key-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a tail-recursive function called contains?
;; that consumes a key and a binary tree (not necessarily a BST)
;; and produces true if the tree contains the key, false otherwise.
;; The function uses tail recursion to efficiently traverse the tree nodes.

(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))

;; BT Integer -> Boolean
;; consumes a binary and a key produces true if the tree contains the given key
(check-expect (contains-key? BT1 6) false)
(check-expect (contains-key? BT2 6) true)
(check-expect (contains-key? BT2 2) false)

(define (contains-key? bt k0)
  (cond [(false? bt) false]
         [else
           (local [(define (contains-key? k todo)
                     (cond [(empty? todo) false]
                           [else
                            (cond [(false? (first todo)) (contains-key? k (rest todo))]
                                  [(equal? (node-k (first todo)) k) true]
                                  [else
                                   (contains-key? k (append (rest todo)
                                                            (list (node-l (first todo)) (node-r (first todo)))))])]))]
             (contains-key? k0 (list bt)))]))
 