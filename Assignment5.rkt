;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;A IBST(binary search tree) is one of:
;;- (make-leaf)
;;- (make-branch Integer BST BST)
(define-struct leaf ())
(define-struct branch (key left right))
;;Constraints:
;;  1. all the keys on the left of the root are less than the root
;;  2. all the keys on the right of the root are greater than the root
;;  3. #1 and # 2 are true for every subtree
;;  4. all the keys are unique

#;(define (ibst-temp abst)
    (cond
      [(leaf? aibst)...]
      [(branch? aibst)...(branch-key aibst)
                     ...(ibst-temp (branch-left aibst))
                     ...(ibst-temp (branch-right aibst))]))

;;Example
(define lf (make-leaf))
(define ibst1 (make-branch 10 
                          (make-branch 8 
                                       (make-branch -1  lf lf)
                                       lf)
                          (make-branch 12 
                                       lf
                                       (make-branch 14 
                                                    (make-branch 13  lf lf)
                                                    lf))))


;;ibst-contains? : IBST Number -> Boolean
;;comsumes an IBST and a number, return true if the number
;;provided is in IBST and false otherwise

(define (ibst-contains? aibst anum)
  (cond
    [(leaf? aibst) #false]
    [(branch? aibst)
     (cond
     [(= (branch-key aibst) anum)#true]
     [(> (branch-key aibst) anum)
      (ibst-contains? (branch-left aibst) anum)]
     [(< (branch-key aibst) anum)
      (ibst-contains? (branch-right aibst) anum)])]))

(check-expect (ibst-contains? lf 1) #false)
(check-expect (ibst-contains? ibst1 10) #true)
(check-expect (ibst-contains? ibst1 -1) #true)
(check-expect (ibst-contains? ibst1 14) #true)
(check-expect (ibst-contains? ibst1 7) #false)

;;ibst-add: IBST Number -> IBST
;;comsumes an IBST and a number, returns a new IBST with
;;the number provided added to the original IBST

(define (ibst-add aibst anum)
  (cond
    [(leaf? aibst)(make-branch anum lf lf)]
    [(branch? aibst)
     