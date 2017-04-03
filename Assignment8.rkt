;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment8) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;Fibonacci: NaturalNum -> NaturalNum
;;consumes a Natural Number and produces its Fibonacci number
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 11) 89)

(define (fibonacci anum)
  (cond
    [(= anum 0) 0]
    [(= anum 1) 1]
    [else (+ (fibonacci(- anum 1)) (fibonacci(- anum 2)))]))

;;Fibonacci-improve:NaturalNum -> NaturalNum
;;consumes a Natural Number and produces its Fibonacci number
(check-expect (fibonacci-improve 0) 0)
(check-expect (fibonacci-improve 1) 1)
(check-expect (fibonacci-improve 2) 1)
(check-expect (fibonacci-improve 11) 89)

(define (fibonacci-improve anum)
  (local [;; NatNum [list-of NatNum] -> [list-of NatNum]
          ;; consumes a Natural Number and produces its Fibonacci number
          ;; ACCUMULATOR: keeps track of the Fibonacci numbers
          ;; produced so far in reverse order
          (define (list-of-FN anum alist)
            (cond
              [( = anum 0) '(0)]
              [( = anum 1) '(1 0)]
              [else(cons
                     (+
                      (first (list-of-FN (- anum 1) alist))
                      (second (list-of-FN (- anum 1) alist)))
                     (list-of-FN (- anum 1) (list-of-FN (- anum 1) alist)))]))]
    (first (list-of-FN anum '()))))

;; A network of Twitter users is a [List-of TwitterUser]
;; In a network, all followers appeared should also be users themselves
;; A Twitter user is a (make-twUser Symbol [List-of Symbol])
(define-struct twUser(name followers))



;;examples of a  network of five users
(define network1 (list (make-twUser 'amy '(bill claire))
                      (make-twUser 'bill '(phil))
                      (make-twUser 'phil '(manney))
                      (make-twUser 'manney '(amy bill))
                      (make-twUser 'claire '())))
(define network2 (list (make-twUser 'amy '(bill claire))
                      (make-twUser 'bill '(phil jay))
                      (make-twUser 'phil '(manney))
                      (make-twUser 'manney '(amy bill phil))
                      (make-twUser 'claire '())
                      (make-twUser 'jay '(manney))))

;;list-handles:[List-of TwitterUser]-> [List-of Symbol]
;;consumes a network and produces a list of all of the handles in the network.
(check-expect (list-handles network1) '(amy bill phil manney claire))
(check-expect (list-handles '()) '())
(define (list-handles alotw)
  (local [;;[List-of TwitterUser][List-of Symbol]-> [List-of Symbol]
          ;;produces a list of all of the handles in the network
          ;;ACCUMULATOR: keeps track of the handle list so far
          (define (handles alotw acc)
            (cond [(empty? alotw) acc]
                  [else (cons (twUser-name (first alotw))
                              (handles (rest alotw)acc))]))]
    (handles alotw '())))

;;most-followers:[List-of TwitterUser] -> Symbol
;;consumes a network and produces the handle in the network
;;that has the most followers.if there is a tie, produces the first handle in
;;[List-of TwitterUser] that has the most followers
(check-expect (most-followers network1) 'amy)
(check-expect (most-followers network2) 'manney)

(define (most-followers alotw)
  (local [;;[List-of TwitterUser] TwitterUser -> TwitterUser
          ;;finds the TwitterUser in the network that has the most followers
          ;;ACCUMULATOR: keeps track of the TwitterUser
          ;;that has the most followers so far
          (define (max alotw acc)
            (foldr (λ(x y) (if (> (follower-number x) (follower-number y))
                               x y)) acc alotw))]
          
   (twUser-name (max alotw (first alotw)))))

;;follower-number: TwitterUser -> Integer
;;determins how many followers a specific Tweeter user has
(check-expect (follower-number (make-twUser 'amy '(bill claire))) 2)
(check-expect (follower-number (make-twUser 'claire '())) 0)
(define (follower-number auser)
   (foldr (λ(x y) (add1 y)) 0  (twUser-followers auser)))

#|;;friends?: [List-of TwitterUser]-> Boolean
;;consumes a network and determins whether it contains 
;;two users who follow each other
#|(check-expect (friends? network1) #false)
(check-expect (friends? network2) #true)
(define (friends? alotw)
  (ormap (λ(x) (member? (twUser-name x) (followers'followers x))) alotw))|#

;;followers'followers: [List-of TwitterUser] TwitterUser -> [List-of Symbol]
;;given a Tweeter user , finds out all of his(her) followers's followers
(check-expect (followers'followers network1 (make-twUser 'amy '(bill claire)))
              '(phil))
(check-expect (followers'followers network1 (make-twUser 'claire '()))
              '())
(check-expect (followers'followers network1 )(make-twUser 'manney '(amy bill))
              '(bill claire phil))

(define (followers'followers alotw auser)
  (cond
    [(empty? (twUser-followers auser)) '()]
    [(cons? (twUser-followers auser))
     (append (first (twUser-followers auser))|#

;;make-palindrome: String -> String
;;consumes a non-empty String and constructs a palindrome
;;by mirroring the String around the last letter
(check-expect (make-palindrome "a") "a")
(check-expect (make-palindrome "fundies") "fundieseidnuf")

(define (make-palindrome astr)
(string-append astr (strreverse (substring astr 0 (-(string-length astr) 1)))))

;;strreverse: String -> String
;;consumes a string and constructs a new string with every letter reversed
(check-expect (strreverse "a") "a")
(check-expect (strreverse "abc")"cba")
(check-expect (strreverse "")"")

(define (strreverse astr)
 (list->string(foldl cons '() (string->list astr))))

;;is-palindrome?:String -> Boolean
;;consumes a non-empty String and determines
;;whether the String is a palindrome or not
(check-expect (is-palindrome? "a") #true)
(check-expect (is-palindrome? "fundieseidnuf") #true)
(check-expect (is-palindrome? "abcda") #false)

(define (is-palindrome? astr)
  (string=?
   (substring astr 0 (/ (-(string-length astr) 1) 2))
   (strreverse
    (substring astr
               (+ (/ (-(string-length astr) 1) 2) 1)
               (string-length astr)))))

;;bsort: [List-of Number] -> [List-of Number]
;;consume a list of numbers and produce the same list of numbers
;;but in sorted order, using the algorithm for bubble sort
;;(check-expect (bsort '()) '())
(check-expect (bsort '( 5 1 4 2 8 )) '( 1 2 4 5 8 ))
(define (bsort alon)
  (local [;;
          (define (sweep alon i swapped?)
            (cond
              [(= i (- n 1))(if swapped? alon (sweep alon 0 #true))]
              [else (if (<= (list-ref alon i)(list-ref alon (+ i 1)))
                         (sweep alon (+ i 1) swapped?)
                         (sweep (swap alon i)(+ i 1) #false))]))
          (define n (length alon))]
    (sweep alon 0 #false)))
;;swap:[List-of Number] -> [List-of Number]
;;swap items i and i+1 in a [List-of Number]
(define (swap alon i)
  (local[;;
         (define 
  (cdr
