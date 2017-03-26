;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a7-add) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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


;;lb->imgae:LegoBldg-> Image
;;takes a lego building and prodecus an image of the building
(check-expect (lb->image
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


(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-lego 6 'orange 60)
                         (make-bigger (make-lego 2 'blue 60)
                                      (make-lego 1 'yellow 40)
                                      (make-lego 3 'red 40))))
                         
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align "bottom"
                                   (rectangle 60 10 'solid 'orange)
                                   (above (rectangle 60 10 'solid 'blue)
                                           (beside/align "baseline"(rectangle 40 10 'solid 'yellow)
                                                       (rectangle 40 10 'solid 'red))))))
                                 



(define (lb->image abigger)
  (cond
    [(lego? abigger) (rectangle (lego-width abigger) 10 'solid (lego-color abigger))]
    [else (above (rectangle (lego-width (bigger-lego abigger)) 10 'solid (lego-color (bigger-lego abigger)))
                 (beside/align (y-pisition? (bigger-left abigger)(bigger-right abigger))
                               (lb->image (bigger-left abigger))
                               (lb->image (bigger-right abigger))))]))

(define (how-high abigger)
  (cond
    [(lego? abigger) 10]
    [else (max
           (+ 10 (how-high (bigger-left abigger)))
           (+ 10 (how-high (bigger-right abigger))))]))

;;y-place?:LegoBldg LegoBldg -> String
;;determins what position the image of the second LegoBldg should be put beside the image of the first LegoBldg at y position
(define (y-pisition? l1 l2)
  (cond [(> (how-high l1) (how-high l2)) "top"]
        [(< (how-high l1) (how-high l2)) "bottom"]
        [(= (how-high l1) (how-high l2)) "baseline"]))


;; A Ball is a (make-ball Nat Mode Color [Nat -> Posn])
(define-struct ball (r mode color placement))
; - where r is the ball's radius
; - mode is the ball's mode
; - color is the ball's color
; - and placement is a function that, given the current time,
; outputs a new coordinate for the ball to be drawn at
; A Mode is one of:
; - 'solid
; - 'outline


(define HEIGHT 500)
(define WIDTH 500)
(define BALL-1 (make-ball 5 'solid 'red (λ (t) (make-posn
20 (modulo t HEIGHT)))))
(define BALL-2 (make-ball 7 'outline 'blue (λ (t) (make-posn
(modulo t WIDTH) 100))))

; A World is a (make-world Nat [List-of Ball])
(define-struct world (t balls))
; - where t is the amount of time that has passed
; - and balls is the balls of the world

(define WORLD-1 (make-world 0 '()))
(define WORLD-2 (make-world 10 (list BALL-1 BALL-2)))


(define (tick w)
  (make-world (+ (world-t w) 1) (world-balls w)))
(check-expect (tick WORLD-1) (make-world 1 '()))

(define (draw-ball b aposn aimg)
  (place-image (circle (ball-r b) (ball-mode b) (ball-color b))
               (posn-x aposn)
               (posn-y aposn)
               aimg))

(check-expect (draw-ball BALL-1 (make-posn 10 20) (empty-scene WIDTH HEIGHT))
              (place-image (circle 5 'solid 'red) 10 20
                           (empty-scene 500 500)))

(define (make-drawer b aimg)
  (draw-ball b (ball-placement b) aimg))
;;how to check about it??????

(define (draw w)
  (foldr (λ(x img) (make-drawer x img))(empty-scene WIDTH HEIGHT)(world-balls w)))

  ; A BallGenerator is a [Nat Nat Nat -> [Nat -> Posn]]
; Given the time, x-coordinate, and y-coordinate of when and where a
; ball is created, create a function that, given the current time of
; the world, will output a Posn
; Example:
; move-horizontally : BallGenerator
(define (move-horizontally t0 x0 y0)
 (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH) y0)))
(check-expect ((move-horizontally 3 5 8) 10) ; 7 seconds have passed
              (make-posn 12 8))
; move-vertically : BallGenerator
(define (move-vertically t0 x0 y0)
  (λ(t)(make-posn x0 (modulo (+ y0 (- t t0)) HEIGHT))))
(check-expect ((move-vertically 3 5 8) 5)
              (make-posn 5 10))

(define GENERATORS '(move-horizontally move-vertically))

(define (place-ball w num1 num2 mouse)
  (mouse-event? mouse)
  (make-world (world-t w) (list* (make-ball 5 'solid 'green (move-horizontally 3 num1 num2))
                                 (world-balls w))))

(define (main init-list)
 (big-bang (make-world 0 init-list)
 [on-tick tick]
 [to-draw draw]
 [on-mouse place-ball]))

(main '(BALL-1 BALL-2))