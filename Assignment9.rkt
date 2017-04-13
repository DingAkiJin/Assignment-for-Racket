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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;find-st-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;find-st-path:Graph Node Node -> [Maybe Path]
;;consumes a graph g, a start node s and an end node t and returns a path p
;;that starts at  s and ends at t in the graph g. if s = t, returns a empty list
;;The function should return false if there is no such path.
;;[Maybe Path] is one of:
;;-[List-of Edge];
;;-#false;

(check-expect (find-st-path g1 'D 'B) #false)
(check-expect (find-st-path g1 'A 'A) '())

(check-expect (find-st-path g1 'A 'C)
              (list (list 'A 10 'B)
                 (list 'B 5 'C)))

(check-expect (find-st-path g1 'A 'D) 
               (list (list 'A 10 'B)
                (list 'B 5 'C)
                (list 'C 10 'D)))
(check-expect (find-st-path g1 'E 'D) #false)
(define (find-st-path g s t)
  (cond
    [(or (not-in-graph? g s) (not-in-graph? g t)) #false]
    [else
  (if (boolean?(find-path-acc g s t empty))
      #false
  (find-edges g (find-path-acc g s t empty)))]))

;;not-in-graph? Graph Node -> Boolean
;;returns true if a node is in a given graph, false otherwise


(check-expect (not-in-graph? g1 'E) #true)
(check-expect (not-in-graph? g1 'A) #false)
(check-expect (not-in-graph? empty 'A ) #true)
(define (not-in-graph? g anode)
  (andmap (λ(x) (and (not (symbol=? (first x) anode))
                     (not(symbol=? (third x) anode)))) g))
                    
;;find-path-acc: Graph Node Node [List-of Node] ->  [Maybe [List-of Node]]
;;consumes a graph g, a start node s and an end node t and a list of nodes
;;returns a list of nodes that starts at  s and ends at t in the graph g. 
;;The function should return false if there is no such path.
;; assuming s t are nodes in the graph  
(check-expect (find-path-acc g1 'D 'B '()) #false)
(check-expect (find-path-acc g1 'A 'A '()) '(A))
(check-expect (find-path-acc g1 'A 'C '())
              (list 'A 'B 'C))

(check-expect (find-path-acc g1 'A 'D '()) 
               (list 'A 'B 'C 'D))
           
(define (find-path-acc g s t seen-so-far)
  (cond [(symbol=? s t) (list t)]
        [(member?  s seen-so-far) #false]
        [else
         (if(boolean?(find-path/list g (neighbors g s) t (cons s seen-so-far)))
          (find-path/list g (neighbors g s) t (cons s seen-so-far))
          (cons s (find-path/list g (neighbors g s) t (cons s seen-so-far))))]))

;;Graph [List-of Node] Node [List-of Node]->[Maybe [List-of Node]]
;;consumes a graph g, a list of nodes(next-nodes) and an end node t
;;and a list of nodes,returns a list of nodes that starts at one of next-nodes
;;and ends at t in the graph g. 
;;The function should return false if there is no such path.
;;assuming next-nodes t are nodes in the graph
(check-expect (find-path/list g1 empty 'D '()) #false)
(check-expect (find-path/list g1 '(A D) 'D '()) '(A B C D))
(check-expect (find-path/list g1 '(B) 'D '()) '(B C D))


(define (find-path/list g next-nodes t seen-so-far)
  (cond [(empty? next-nodes) #false]
        [else (if (boolean? (find-path-acc g (first next-nodes) t seen-so-far))
                  (find-path/list g (rest next-nodes) t seen-so-far)
                  (find-path-acc g (first next-nodes) t seen-so-far))]))

;;neighbors: Node Graph -> [List-of Node]
;;finds all nodes that a given node in a graph has an edge with
(check-expect (neighbors g1 'A) '(B))
(check-expect (neighbors g1 'C) '(A D))
(check-expect (neighbors g1 'D) '())
(define (neighbors g node)
  (foldr (λ(x y) (if (symbol=? (first x) node)
                     (cons  (third x) y)
                      y)) '() g))


;;find-edge: Graph Node Node -> Edge
;;finds the edge of two different nodes in a graph,
;;assuming the edge always exists
(check-expect (find-edge g1 'A 'B) (list(list 'A 10 'B)))
(check-expect (find-edge g1 'C 'A) (list(list 'C 10 'A)))
(check-expect (find-edge g1 'B 'D) '())
(define (find-edge g s t)
  ;;[Edge -> Boolen] [List-of Edge] -> [List-of Edge]
  (filter (λ(x) (and(symbol=? (first x) s)(symbol=? (third x) t))) g))

;;find-edges: Graph [List-of Node] -> [List-of Edge]
;;finds the edges in a graph given a list of nodes

(check-expect (find-edges g1 '()) #false)
(check-expect (find-edges g1 '(A)) '())
(check-expect (find-edges g1 '(A B C))
              (list (list 'A 10 'B)
                    (list 'B 5 'C)))

(define (find-edges g alon)
  (cond
    [(empty? alon) #false]
    [(empty? (rest alon)) '()]
    [else (append (find-edge g (first alon) (first (rest alon)))
                (find-edges g (rest alon)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|(define g2 (list (list 'A 10 'B)
                 (list 'B 5 'C)
                 (list 'C 10 'A)
                 (list 'A 10 'C)
                 (list 'C 10 'D)))
(define (find-paths origination destination G)
  (local (define (find-paths-ac orig seen)
            (cond
              [(symbol=? orig destination) (list (list destination))]
              [(member orig seen)#false]   ; you can also return an error here. 
              [else (local[ 
                     
                       (define candidate 
                         (find-paths/list (neighbors g orig) (cons orig seen)))]
                      (join orig candidate))]))

          (define (find-paths/list lo-Os seen)  
            (cond [(empty? lo-Os) #false]
                  [else (local ((define candidate (find-paths-ac (first lo-Os) seen)))
                          (cond [(empty? candidate) (find-paths/list (rest lo-Os) seen)]
                                [else 
                                  (append candidate
                                    (find-paths/list (rest lo-Os) seen))]))]))) 
    (find-paths-ac origination empty))|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define g2 (list (list 'A 10 'B)
                 (list 'B 5 'C)
                 (list 'C 10 'A)
                 (list 'A 10 'C)
                 (list 'C 10 'D)))
;;(check-expect (find-all-path g2 'A 'C '()) (list (list 'A 'B 'C)
                                                 ;;(list 'A 'C)))
(check-expect (find-all-path g2 'B 'C '()) (list (list 'B 'C)))

(define (find-all-path g s t seen-so-far)
  (cond [(symbol=? s t) (list t)]
        [(member?  s seen-so-far) #false]
        [else
         (if(boolean?(find-all-path/list g (neighbors g s) t (cons s seen-so-far)))
          (find-all-path/list g (neighbors g s) t (cons s seen-so-far))
          (cons s (find-all-path/list g (neighbors g s) t (cons s seen-so-far))))]))




(define (find-all-path/list g next-nodes t seen-so-far)
  (cond [(empty? next-nodes) '()]
        [else (if (boolean? (find-all-path g (first next-nodes) t seen-so-far))
                  (find-all-path/list g (rest next-nodes) t seen-so-far)
                  (append
                   (find-all-path g (first next-nodes) t seen-so-far)
                   (find-all-path/list g (rest next-nodes) t seen-so-far)))]))