;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname accounts-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; This program defines an abstract function called fold-act
;; that consumes a collection of accounts and a combining function,
;; and processes the accounts to produce a result.

;; Using fold-act, it redefines remove-debtors and remove-profs
;; to filter out accounts based on specific criteria without changing their signatures or tests.

;; It also defines remove-odd-characters using fold-act,
;; which removes accounts whose holder's name length is odd.

;; For account lookup by ID, using fn-for-acts template is better
;; because it fits searching for a single element,
;; whereas fold-act is more suited for aggregation or transformation of the entire collection.

(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


;; Accounts -> Accounts
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))


(define (remove-debtors act)
  (local [(define (is-debtor? act) (< (node-bal act) 0))]
    (remove-if is-debtor? act)))


;; Accounts -> Accounts
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))


(define (remove-profs act)
  (local [(define (is-prof? act) (has-prefix? "Prof." (node-name act)))]
    (remove-if is-prof? act)))

;;===========================================
;; (Account -> Boolean) Account -> Account
;; removes any subaccount in the account tree that return true on predicate and joins the rest of the subaccounts in the tree together
(check-expect (local [(define (is-prof? act) (has-prefix? "Prof." (node-name act)))]
                (remove-if is-prof? (make-node 27 "Mr. Smith" 100000 false false)))
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (local [(define (is-prof? act) (has-prefix? "Prof." (node-name act)))]
                (remove-if is-prof? (make-node 27 "Prof. Smith" 100000 false false)))
              false)
(check-expect (local [(define (is-debtor? act) (< (node-bal act) 0))]
                (remove-if is-debtor? (make-node 1 "Mr. Rogers" 22 false false))) 
              (make-node 1 "Mr. Rogers" 22 false false))
(check-expect (local [(define (is-debtor? act) (< (node-bal act) 0))]
                (remove-if is-debtor? (make-node 14 "Mr. Impossible" -9 false false))) 
              false)

(define (remove-if pred act)
  (cond [(false? act) false]
        [else
         (if (pred act)
             (join (remove-if pred (node-l act))
                   (remove-if pred (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-if pred (node-l act))
                        (remove-if pred (node-r act))))]))

;;==============================================
;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))

;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


;;




;; Account -> Account
;; From an account tree removes any account with a name that has an odd number of characters
(check-expect (remove-odd ACT1) ACT1)
(check-expect (remove-odd ACT42)
              (make-node 50 "Miss 604"  16 (make-node 27 "Mr. Selatcia" 40 (make-node 14 "Mr. Impossible" -9 false false) false) false))

(define (remove-odd act)
  (local [(define (is-odd? act) (equal? (modulo (string-length (node-name act)) 2) 1))]
    (remove-if is-odd? act)))

  

;; (Number String Number Y Y -> Y) Y Account -> Y 
;; the fold function for Account

(define (fold-act fn i act) ;; Y
  (cond [(false? act) i]
        [else
         (fn (node-id act)
             (node-name act)
             (node-bal act)
             (fold-act fn i (node-l act))
             (fold-act fn i (node-r act)))]))

;; Account -> Account
;; decrements the balance of every account in the account tree
(check-expect (decrement-balance ACT42)
              (make-node 42 "Mr. Mom" -82
                         (make-node 27 "Mr. Selatcia" 37  
                                    (make-node 14 "Mr. Impossible" -12 false false)
                                    false)
                         (make-node 50 "Miss 604"  13 false false)))

(define (decrement-balance act)
  (local [(define (decrement-single id name bal lact ract) (make-node id name (- bal 3) lact ract))]
    (fold-act decrement-single false act)))

  

;; the template is easier to use because it can allow you to stop when the id is found while the fold function will process every value 
  