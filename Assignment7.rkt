;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment7) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; An Atom is one of:
;; -Number
;; -Symbol
;; -String

;;An SExp is one of:
;; - Atom
;; -[List-of SExp]

;; A [List-of SExp] is one of:
;; - '()
;; - (cons S-exp LoS-exp)

;;sexp->string: SExp -> String
;;consumes an s-expression and produce a string 
;;with the textual representation of that s-expression
(check-expect (sexp->string '(a(37"foo")c)) "(a(37\"foo\" )c )");;why space matters??
(check-expect (sexp->string 'a) "a")
(check-expect (sexp->string 1) "1")
(check-expect (sexp->string "nice")  "\"nice\"")
(check-expect (sexp->string '()) "( )")

(define (sexp->string asexp)
  (cond
    [(number? asexp) (number->string asexp)]
    [(symbol? asexp) (symbol->string asexp)]
    [(string? asexp) (string-append "\"" asexp "\"")]
    [else (string-append "("(losexp->string asexp)")")]))

;;losexp->string: SExp -> String
;;when an SExp is a [List-of SExp],represents it like a string
(define (losexp->string asexp)
  (cond
    [(empty? asexp)" "]
    [else (string-append (sexp->string (first asexp))
                         (losexp->string (rest asexp)))]))


;;sexp-fn: SExps SExps -> ??
#;(define (sexp-fn s1 s2)
    (cond
      [(and (number? s1) (number? s2))...]
      [(and (symbol? s1) (symbol? s2))...]
      [(and (string? s1) (string? s2))...]
      [else (losexp-fn s1 s2)]))

;;losexp-fn:[List-of SExp] [List-of SExp] -> ??
#;(define (losexp-fn los1 los2)
    (cond [(and (empty? los1)(empty? los2))...]
          [(and (cons? los1) (cons? los2))
               ...(sexp-fn (first los1))
               ...(sexp-fn (first los2))
               ...(losexp-fn (rest los1))
               ...(losexp-fn (rest los2))]))
  
#|;;contains-same-atoms?: SExps SExps -> Boolean
;;determine if two SExps contain the same atoms regardless of the ordering
(check-expect (contains-same-atoms? '(1 2 3 () ("r" b)) '("r" 1 (2) 3 b)) #true)
(check-expect (contains-same-atoms? '(1 2 3 ("r" b)) '(1 2 3)) #false)
(check-expect (contains-same-atoms? '() '(1 2)) #false)
(check-expect (contains-same-atoms? '()'())#true)

(define (contains-same-atoms? s1 s2)
  (cond
    [(number? s1)(and (number? s2)(= s1 s2))]
    [(symbol? s1)(and(symbol? s2)(symbol=? s1 s2))]
    [(string? s1) (and(string? s2)(string=? s1 s2))]
    [else (contains-same-atoms-in-list? s1 s2)]))


;;contains-same-atoms-in-list1?: [List-of SExp] [List-of SExp] -> Boolean
;;determine if two [List-of SExp] contain the same atoms
;;regardless of the ordering
(define (contains-same-atoms-in-list1? s1 s2)
  (cond
    [(empty? s1)(empty? s2)]
    [(cons? s1)
     (and
      (cons? s2)
      (or(contains-same-atoms? (first s1)  s2)
         (contains-same-atoms-in-list? (rest s1) s2)))]))|#


(define-struct lego(label color width))
;;A Lego is a structure:
;;   (make-lego Number Symbol Number)
;;interpretation: (make-lego l c w) is the lego brick
;;with label l, color c, and width w(in pixels).

(define-struct bigger (lego left right))
;;A LegoBldg (lego building) is one of:
;;-Lego
;;-(make-bigger Lego LegoBldg LegoBldg)
;;interpretation: (make-bigger lg lft rgt)makes a bigger
;;lego building by putting a lego brick lg on top of two lego
;;buildings lft(left) and rgt(right).

;;count-bricks: LegoBldg -> Number
;;taks a lego building and produces the total number
;;of lego bricks in that building
(define LEGO (make-lego 0 'red 10))
(check-expect (count-bricks
               (make-bigger LEGO
                            (make-bigger LEGO LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))))11)
(check-expect (count-bricks LEGO) 1)

(define (count-bricks abigger)
  (cond
    [(lego? abigger) 1]
    [else (+ 1
             (count-bricks (bigger-left abigger))
             (count-bricks (bigger-right abigger)))]))

;;how-high:LegoBldg -> Number
;;taks a lego building and produces the height of the lego building
(check-expect (how-high
               (make-bigger LEGO
                            (make-bigger LEGO LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))))40)
(check-expect (how-high LEGO) 10)

(define (how-high abigger)
  (cond
    [(lego? abigger) 10]
    [else (max
           (+ 10 (how-high (bigger-left abigger)))
           (+ 10 (how-high (bigger-right abigger))))]))

;;contains-colored-brick?: LegoBldg Symbol -> Boolean
;;taks a lego building and a color , determins whether
;;the building contains a lego brick of the given color
(check-expect (contains-colored-brick?
               (make-bigger LEGO
                            (make-bigger LEGO LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))) 'red)
              #true)
(check-expect (contains-colored-brick?
               (make-bigger LEGO
                            (make-bigger LEGO LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))) 'green)
              #false)
(check-expect (contains-colored-brick? LEGO 'red) #true)
(check-expect (contains-colored-brick? LEGO 'green) #false)

(define (contains-colored-brick? abigger acolor)
  (cond
    [(lego? abigger) (symbol=? (lego-color abigger) acolor)]
    [else (or
           (symbol=? (lego-color (bigger-lego abigger)) acolor)
           (contains-colored-brick? (bigger-left abigger) acolor)
           (contains-colored-brick? (bigger-right abigger) acolor))]))

;;find-colored-brick?:LegoBldg Symbol -> MaybeLego
;;taks a lego building and a color and finds any lego with the given color
;;in the building, or returns false if there are no such legos................can to make sure I stop and not repeatly run the program

;;A MaybeLego is one of:
;;-false
;;-Lego
(check-expect (find-colored-brick?
               (make-bigger LEGO
                            (make-bigger (make-lego 1 'green 10)
                                         (make-lego 2 'green 11)
                                         LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))) 'green)
       (make-lego 1 'green 10))
(check-expect (find-colored-brick?
               (make-bigger LEGO
                            (make-bigger LEGO
                                         (make-lego 2 'green 11)
                                         LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO
                                                   (make-lego 2 'green 10)    
                                                      LEGO)
                                         (make-bigger LEGO LEGO LEGO))) 'green)
       (make-lego 2 'green 11))

(check-expect (find-colored-brick?
               (make-bigger LEGO
                            (make-bigger LEGO  LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO
                                                   (make-lego 2 'green 10)    
                                                   (make-lego 2 'green 11) )
                                         (make-bigger LEGO LEGO LEGO))) 'green)
       (make-lego 2 'green 10))
(check-expect (find-colored-brick?
               (make-bigger LEGO
                            (make-bigger LEGO LEGO LEGO)
                            (make-bigger LEGO
                                         (make-bigger LEGO LEGO LEGO)
                                         (make-bigger LEGO LEGO LEGO))) 'green)
              #false)
(check-expect (find-colored-brick? LEGO 'red)LEGO)
(check-expect (find-colored-brick? LEGO 'green) #false)

(define (find-colored-brick? abigger acolor)
  (cond
    [(lego? abigger)(if(symbol=? (lego-color abigger) acolor)
     abigger
     #false)]
    [else
     (if (symbol=? (lego-color (bigger-lego abigger)) acolor)
         (bigger-lego abigger)
         (if (equal? (find-colored-brick? (bigger-left abigger) acolor) #false)
              (find-colored-brick? (bigger-right abigger) acolor)
             (find-colored-brick? (bigger-left abigger) acolor)))]))

;;lb->imgae:LegoBldg-> Image
;;takes a lego building and prodecus an image of the building
#|(check-expect (lb->imgae
               (make-bigger (make-lego 4 'purple 80)
                            (make-bigger (make-lego 2 'blue 60)
                                         (make-lego 1 'yellow 40)
                                         (make-lego 3 'red 40))
                            (make-bigger (make-lego 6 'orange 60)
                                         (make-lego 5 'green 40)
                                         (make-lego 7 'red 40))))
            (above (rectangle 80 10 'solid 'purple)
                                 (beside/align "baseline"
                                         (above (rectangle 60 10 'solid 'blue)
                                         (beside/align "baseline"(rectangle 40 10 'solid 'yellow)
                                                       (rectangle 40 10 'solid 'red)))
                                        (above (rectangle 60 10 'solid 'orange)
                                         (beside/align "baseline" (rectangle 40 10 'solid 'green)
                                                       (rectangle 40 10 'solid 'red))))))


(check-expect (lb->image (make-lego 4 'purple 80)
                         (make-bigger (make-lego 2 'blue 60)
                                      (make-lego 1 'yellow 40)
                                      (make-lego 3 'red 40))
                         (make-lego 6 'orange 60))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "top"
                                   (above (rectangle 60 10 'solid 'blue)
                                           (beside/align "baseline"(rectangle 40 10 'solid 'yellow)
                                                       (rectangle 40 10 'solid 'red)))
                                   (rectangle 60 10 'solid 'orange))))|#



 (above (rectangle 80 10 'solid 'purple)
                     (beside/align "baseline"
                                   (above (rectangle 60 10 'solid 'blue)
                                           (beside/align "baseline"(rectangle 40 10 'solid 'yellow)
                                                       (rectangle 40 10 'solid 'red)))
                                   (rectangle 60 10 'solid 'orange)))