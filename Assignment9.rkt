;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment9) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A Node is a Symbol 
;; INTERP: represents the name of a node in a graph

;; A Distance is a PosInt
;; INTERP: represents distance in miles

;; An Edge is (list Node Distance Node)
;; e.g. (list 'A 10 'B)
;; INTERP: represents an edge from 'A to 'B with the distance from 'A to 'B
;;being 10 miles



;; A Path is a [List-of Edge]
;; A Graph is a [Set-of Edge]
;; NOTE: you can use the definition of Set from your previous assignment. 

(define g1 (list (list 'A 10 'B)
                 (list 'B 5 'C)
                 (list 'C 10 'A)
                 (list 'C 10 'D)))
(define p1 (list (list 'A 10 'B)
                 (list 'B 5 'C)
                 (list 'C 10 'D)))
(define p2 (list (list 'D 10 'C)))
(define p3 (list (list 'E 5 'A)
                 (list 'A 10 'B)))
(define p4 (list (list 'A 10 'B)
                 (list 'B 5 'C)
                 (list 'C 10 'A)))
(define p5 (list (list 'A 10 'B)
                (list 'C 5 'A)))
                
;;valid-path?: Graph Path-> Boolean
;; consumes a graph and a path, and returns true
;;if the path is valid for the graph, and false otherwise.
;;i.e., the graph contains these edges from the path
;;and we can follow them in the order given by the path.
;;;(check-expect (valid-path? g1 p1) #true)
(check-expect (valid-path? g1 p2) #false)
(check-expect (valid-path? g1 p3) #false)
(check-expect (valid-path? '() p1) #false)
(check-expect (valid-path? g1 p4) #true)
(check-expect (valid-path? g1 p5) #false)

(define (valid-path? g p)
  (and
   (andmap (λ(x) (member? x g)) p)
   (valid? p)))

;;valid?: Path-> Boolean
;;comsumes a path, if we can follows the edges in the order
;;return #true, otherwise, #false
(check-expect (valid? p1) #true)
(check-expect (valid? p5) #false)
(check-expect (valid? '()) #false)
(check-expect (valid? '((list 'A 10 'B))) #true)

(define (valid? p)
  (cond
    [(empty? p) #false]
    [(empty? (rest p)) #true]
    [(cons? (rest p))
     (if (symbol=? (third(first p)) (first (second p)))
         (valid? (rest p))
         #false)]))
#|;;n-position-in-graph: Graph Edge-> NonnegativeNumber
;;finds out the position of edge in a graph that is equal with an given edge
;;when it exists in the graph
(check-expect (n-position-in-graph g1 (list 'A 10 'B)) 0)
(check-expect (n-position-in-graph g1 (list 'C 10 'A)) 2)

(define (n-position-in-graph g e)
     (local [;; Graph Edge Number -> Number
             ;; finds out the position of edge in a graph that is equal
             ;; with an given edge
             ;; ACCUMULATOR: keeps track of the positon so far
             (define (position g e acc)
               (foldr (λ(x y) (if (equal? x e) acc (+ 1 y))) acc g))]
       (position g e 0)))

;;ascending?: [List-of Integer] -> Boolean
;;Tells whether a list of integers are in ascending order
(check-expect (ascending? '()) #true)
(check-expect (ascending? '(1)) #true)
(check-expect (ascending? '(1 2 3)) #true)
(check-expect (ascending? '(3 1)) #false)

(define (ascending? alon)
  (cond
    [(empty? alon) #true]
    [(empty? (rest alon)) #true]
    [else (and(= (+ (first alon) 1) (first (rest alon)))
              (ascending? (rest alon)))]))|#

;;valid-st-path?: Graph Node Node Path -> Boolean
;;consumes a graph g, a start node s, an end node t, and a path p, and returns
;;true if starting at node s in g and following, in order, the edges in p,
;;will lead us to t.The function should return false otherwise.
(check-expect (valid-st-path? g1 'A 'B p2) #false)
(check-expect (valid-st-path? g1 'C 'B p1) #false)
(check-expect (valid-st-path? g1 'A 'D (list (list 'B 5 'C))) #true)

(define (valid-st-path? g s t p)
  (and
    (valid-path? g p)
    (ormap (λ(x) (and (symbol=? (first x) s)
                      (symbol=? (third x) (first (first p))))) g)
    (ormap (λ(y) (and (symbol=? (first y) (third (last p)))
                      (symbol=? (third y) t))) g)))

;;last: [List-of Any]-> Any
;;finds out the last item in a list
(check-expect (last '(1 2 3 4)) 4)
(check-expect (last '()) '())
(check-expect (last '(1 a b c)) 'c)

(define (last alist)
  (cond
    [(empty? alist) empty]
    [else
     (if (empty? (rest alist))
         (first alist)
         (last (rest alist)))]))

;;find-st-path: Graph Node Node -> [Maybe Path]
;;consumes a graph g, a start node s and an end node t and returns a path p
;;that starts at  s and ends at t in the graph g.
;;The function should return false if there is no such path.
;;[Maybe Path] is one of:
;;-Path;
;;-#false;
(check-expect (find-st-path g1 'A 'C)(list (list 'A 10 'B)
                                           (list 'B 5 'C)))

(define (find-st-path g s t)
  (cond
    [(symbol=? s t)'()]
    [else
         (cond
           [(boolean? (find-path/list g (neighbors s g) t ))#false]
            ;;[else(cons s (find-path/list g (neighbors s g) t ))])]))
            [else
              (if(boolean? (find-st-path g (first (neighbors s g)) t ))
                (find-path/list g (rest (neighbors s g)) t)
                (cons (first(find-edge g s (first (neighbors s g))))
                      (find-path/list g (rest (neighbors s g)) t)))])]))
;;find-edge: Graph Node Node -> Edge
;;finds the edge of two different nodes in a graph,
;;assuming the edge always exists
(check-expect (find-edge g1 'A 'B) (list(list 'A 10 'B)))
(check-expect (find-edge g1 'C 'A) (list(list 'C 10 'A)))
(check-expect (find-edge g1 'B 'D) '())
(define (find-edge g s t)
  (filter (λ(x) (and(symbol=? (first x) s)(symbol=? (third x) t))) g))



 
;find-path/list: Graph [List-of Node] Node  -> [Maybe Path]
; finds a path from some node on neigbors to the destination
; if there is no path, the function produces #false
(define (find-path/list g alon t)
  (cond
    [(empty? alon) #false]
    [else (cond
              [(boolean? (find-st-path g (first alon) t))
               (find-path/list g (rest alon) t )]
              [else(find-st-path g (first alon) t)])]))

;;neighbors: Node Graph -> [List-of Node]
;;finds all nodes that a given node in a graph has an edge with
(check-expect (neighbors 'A g1) '(B))
(check-expect (neighbors 'C g1) '(A D))
(check-expect (neighbors 'D g1) '())
(define (neighbors node g)
  (foldr (λ(x y) (if (symbol=? (first x) node)
                     (cons  (third x) y)
                      y)) '() g))
 
