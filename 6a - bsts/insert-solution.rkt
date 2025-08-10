;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname insert-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This program defines a function that consumes an Integer key, a String value, and a BST,
;; and produces a new BST with a node containing the given key and value inserted
;; in the correct position according to BST ordering rules.
;; It assumes the key is not already in the tree and does not perform balancing.

;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))


#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

;; BST -> BST
;; adds a given key and value pair to a BST while preserving the invariant
(check-expect (insert-node BST0 10 "lol") (make-node 10 "lol" false false))
(check-expect (insert-node BST10 5 "kek") (make-node 10
                                                     "why"
                                                     (make-node 3
                                                                "ilk"
                                                                (make-node 1
                                                                           "abc"
                                                                           false
                                                                           false)
                                                                (make-node 4
                                                                           "dcj"
                                                                           false
                                                                           (make-node 7
                                                                                      "ruf"
                                                                                      (make-node 5
                                                                                                 "kek"
                                                                                                 false
                                                                                                 false)
                                                                                      false)))
                                                     (make-node 42
                                                                "ily"
                                                                (make-node 27
                                                                           "wit"
                                                                           (make-node 14
                                                                                      "olp"
                                                                                      false
                                                                                      false)
                                                                           false)
                                                                false)))

;; <Template according to BST, with 2 atomic paramets: Number, String>
(define (insert-node t k v)
  (cond [(false? t) (make-node k v false false)]
        [else
         (if (< k (node-key t))
             (make-node (node-key t)
                        (node-val t)
                        (insert-node (node-l t) k v)
                        (node-r t))
             (make-node (node-key t)
                        (node-val t)
                        (node-l t)
                        (insert-node (node-r t) k v)))]))