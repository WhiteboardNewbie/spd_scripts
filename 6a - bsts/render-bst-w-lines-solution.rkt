;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname render-bst-w-lines-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; This program defines a function that consumes a binary search tree (BST)
;; and produces a simple visual rendering of the tree as an image,
;; including the nodes with their key values and lines connecting each node to its left and right subtrees.
;; The design uses two helpers:
;; - one to render the key value as an image
;; - one to draw connecting lines between nodes and subtrees
;; Each helper produces a rectangular image, allowing composition and layout calculations
;; based on the widths of left and right subtree images.

;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEPARATOR ":")
(define MTTREE (rectangle 20 1 "solid" "white"))


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
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))
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

;; BST -> Image
;; Renders a BST
(check-expect (render-bst BST0) MTTREE)
(check-expect (render-bst BST3) (above (render-node (node-key BST3)
                                                    (node-val BST3))
                                       (render-lines (image-width (render-bst (node-l BST3)))
                                                     (image-width (render-bst (node-r BST3))))
                                       (beside (render-bst (node-l BST3))
                                               (render-bst (node-r BST3)))))

;; <Template taken from BST
(define (render-bst t)
  (cond [(false? t) MTTREE]
        [else
         (above (render-node (node-key t)
                             (node-val t))
                
                (render-lines (image-width (render-bst (node-l t)))
                              (image-width (render-bst (node-r t))))
                (beside (render-bst (node-l t))
                        (render-bst (node-r t))))]))

;; Integer String -> String
;; renders a single node
(check-expect (render-node 10 "abc")
              (text (string-append (number->string 10)
                                   KEY-VAL-SEPARATOR
                                   "abc")
                    TEXT-SIZE
                    TEXT-COLOR))

(define (render-node k v)
  (text (string-append (number->string k)
                       KEY-VAL-SEPARATOR
                       v)
        TEXT-SIZE
        TEXT-COLOR))

;; Integer Integer -> Image
;; renders the lines between nodes
(check-expect (render-lines 10 20)
              (add-line (add-line (rectangle (+ 10
                                                20)
                                             (/ (+ 10
                                                   20)
                                                4)
                                             "solid"
                                             "white")
                                  (/ (+ 10
                                        20)
                                     2)
                                  0
                                  (/ 10
                                     2)
                                  (/ (+ 10
                                        20)
                                     4)
                                  "black")
                        (/ (+ 10
                              20)
                           2)
                        0
                        (+ 10
                           (/ 20
                              2))
                        (/ (+ 10
                              20)
                           4)
                        "black"))

(define (render-lines lw rw)
  (add-line (add-line (rectangle (+ lw
                                    rw)
                                 (/ (+ lw
                                       rw)
                                    4)
                                 "solid"
                                 "white")
                      (/ (+ lw
                            rw)
                         2)
                      0
                      (/ lw
                         2)
                      (/ (+ lw
                            rw)
                         4)
                      "black")
            (/ (+ lw
                  rw)
               2)
            0
            (+ lw
               (/ rw
                  2))
            (/ (+ lw
                  rw)
               4)
            "black"))