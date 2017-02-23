;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |assignment4(edited)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define RADIUS 20)
(define GRID-WIDTH (* RADIUS 3))
(define PLAYER-IMG (circle RADIUS "solid" "green"))
(define HEIGHT (* 7 GRID-WIDTH))
(define WIDTH (* 16 GRID-WIDTH))
(define SCENE (empty-scene WIDTH HEIGHT))
(define BASE (rectangle (* 16 GRID-WIDTH)(* 1 GRID-WIDTH)"solid" "brown"))
(define ROAD-LINE (rectangle (* 16 GRID-WIDTH)(* 1 GRID-WIDTH)"outline""black"))
(define VEHICLE-IMG (rectangle (* 2.5 RADIUS)(* 2 RADIUS) "solid" "red"))
(define BACKGROUND (place-image
                    BASE
                    (* 8 GRID-WIDTH)
                    (* 0.5 GRID-WIDTH)
                    (place-image
                     BASE
                     (* 8 GRID-WIDTH)
                     (* 6.5 GRID-WIDTH)
                     (place-image
                      ROAD-LINE
                      (* 8 GRID-WIDTH)
                      (* 1.5 GRID-WIDTH)
                      (place-image
                       ROAD-LINE
                       (* 8 GRID-WIDTH)
                       (* 2.5 GRID-WIDTH)
                       (place-image
                        ROAD-LINE
                        (* 8 GRID-WIDTH)
                        (* 3.5 GRID-WIDTH)
                        (place-image
                         ROAD-LINE
                         (* 8 GRID-WIDTH)
                         (* 4.5 GRID-WIDTH)
                         (place-image
                          ROAD-LINE
                          (* 8 GRID-WIDTH)
                          (* 5.5 GRID-WIDTH)
                          SCENE))))))))        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Data definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A World is a (make-world Player Lov)
;;A Lov is a list of Vset
;;The VSet represents the set of vehicles moving across the screen
(define-struct world (player lov))

;;A Player is a (make-player Integer Integer Direction)
(define-struct player (x y dir))

;;A Vehicle is a (make-vehicle Integer Integer Direction)
(define-struct vehicle (x y dir))

;;A list of Vset(Lov) is one of:
;;- empty
;;-(cons Vset Lov)

;;A set of Vehicles(Vset) is one of:
;;- empty
;;- (cons Vehicle Vset)

;;A Direction is one of: "left" "right" "up" "down"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;world-fn: World -> ?
#;(define (world-fn w)
    ...(player-fn (world-player w))
    ...(Vehicle-fn(vset-fn (world-vest w)) ))

;;player-fn: Player-> ?
#;(define (player-fn pl)
    ...(player-x pl)
    ...(player-y pl)
    ...(dir-fn (play-dir pl)))

;;dir-fn: Direction ->?
#;(define (dir-fn adir)
    (cond[(string=? adir "left")...]
         [(string=? adir "right")...]
         [(string=? adir "up")...]
         [(string=? adir "down")...]))
;;lov-fn: Lov -> ?
#;(define (lov-fn alov)
    (cond [(empty? alov)...]
          [(cons? alov)...(vehicle-fn(vset-fn (first alov)))
                       ...(lov-fn (rest alov))]))

;;vset-fn: Vset -> ?
#;(define (vset-fn avset)
    (cond [(empty? avset)...]
          [(cons? avest)...(vehicle-fn(first avset))
                        ...(vset-fn (rest avset))]))

;;vehicle-fn: Vehicle -> ?
#;(define (vehicle-fn vl)
    ...(vehicle-x vl)
    ...(vehicle-y vl)
    ...(dir-fn(vehicle-dir vl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Examples:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define player0 (make-player 7.5 6.5 "up"))
(define vset0-road1 (cons (make-vehicle 14 1.5 "right")
                          (cons (make-vehicle 10 1.5 "right")
                                (cons (make-vehicle 6 1.5 "right")
                                      (cons(make-vehicle 2 1.5 "right")
                                           '())))))
(define vset0-road2 (cons (make-vehicle 2 2.5 "left")
                          (cons (make-vehicle 6 2.5 "left")
                                (cons (make-vehicle 10 2.5 "left")
                                      (cons (make-vehicle 14 2.5 "left")
                                            '())))))
(define vset0-road3 (cons (make-vehicle 14 3.5 "right")
                          (cons (make-vehicle 10 3.5 "right")
                                (cons (make-vehicle 6 3.5 "right")
                                      (cons (make-vehicle 2 3.5 "right")
                                            '())))))
(define vset0-road4 (cons (make-vehicle 2 4.5 "left")
                          (cons (make-vehicle 6 4.5 "left")
                                (cons (make-vehicle 10 4.5 "left")
                                      (cons (make-vehicle 14 4.5 "left")
                                            '())))))
(define vset0-road5 (cons (make-vehicle 14 5.5 "right")
                          (cons (make-vehicle 10 5.5 "right")
                                (cons (make-vehicle 6 5.5 "right")
                                      (cons (make-vehicle 2 5.5 "right")
                                            '())))))
(define lov0 (cons vset0-road1
                   (cons vset0-road2
                         (cons vset0-road3
                               (cons vset0-road4
                                     (cons vset0-road5 '()))))))
(define world0 (make-world player0 lov0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Launches the froger game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;World -> World
;;launches the frogger game
(define (main w)
  (big-bang w
            [to-draw draw-world]
            [on-tick move-world 2.0]
            [on-key change-dir]
            [stop-when (if
                        (collision? w)
                        (show-gameover w)
                        (show-youwin w))]))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Drawing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;draw-world:World-> Image
;;draw the current world with a player and a Lov
(check-expect (draw-world world0)
              (place-image
               PLAYER-IMG
               (* 7.5 GRID-WIDTH)
               (* 6.5 GRID-WIDTH)
               (place-image
                VEHICLE-IMG
                (* 2 GRID-WIDTH)
                (* 1.5 GRID-WIDTH)
                (place-image
                 VEHICLE-IMG
                 (* 6 GRID-WIDTH)
                 (* 1.5 GRID-WIDTH)
                 (place-image
                  VEHICLE-IMG
                  (* 10 GRID-WIDTH)
                  (* 1.5 GRID-WIDTH)
                  (place-image
                   VEHICLE-IMG
                   (* 14 GRID-WIDTH)
                   (* 1.5 GRID-WIDTH)
                   (place-image
                    VEHICLE-IMG
                    (* 2 GRID-WIDTH)
                    (* 2.5  GRID-WIDTH)
                    (place-image
                     VEHICLE-IMG
                     (* 6 GRID-WIDTH)
                     (* 2.5 GRID-WIDTH)
                     (place-image
                      VEHICLE-IMG
                      (* 10 GRID-WIDTH)
                      (* 2.5 GRID-WIDTH)
                      (place-image
                       VEHICLE-IMG
                       (* 14 GRID-WIDTH)
                       (* 2.5 GRID-WIDTH)
                       (place-image
                        VEHICLE-IMG
                        (* 2 GRID-WIDTH)
                        (* 3.5 GRID-WIDTH)
                        (place-image
                         VEHICLE-IMG
                         (* 6 GRID-WIDTH)
                         (* 3.5 GRID-WIDTH)
                         (place-image
                          VEHICLE-IMG
                          (* 10 GRID-WIDTH)
                          (* 3.5 GRID-WIDTH)
                          (place-image
                           VEHICLE-IMG
                           (* 14 GRID-WIDTH)
                           (* 3.5 GRID-WIDTH)
                           (place-image
                            VEHICLE-IMG
                            (* 2 GRID-WIDTH)
                            (* 4.5 GRID-WIDTH)
                            (place-image
                             VEHICLE-IMG
                             (* 6 GRID-WIDTH)
                             (* 4.5 GRID-WIDTH)
                             (place-image
                              VEHICLE-IMG
                              (* 10 GRID-WIDTH)
                              (* 4.5 GRID-WIDTH)
                              (place-image
                               VEHICLE-IMG
                               (* 14 GRID-WIDTH)
                               (* 4.5 GRID-WIDTH)
                               (place-image
                                VEHICLE-IMG
                                (* 2 GRID-WIDTH)
                                (* 5.5 GRID-WIDTH)
                                (place-image
                                 VEHICLE-IMG
                                 (* 6 GRID-WIDTH)
                                 (* 5.5 GRID-WIDTH)
                                 (place-image
                                  VEHICLE-IMG
                                  (* 10 GRID-WIDTH)
                                  (* 5.5 GRID-WIDTH)
                                  (place-image
                                   VEHICLE-IMG
                                   (* 14 GRID-WIDTH)
                                   (* 5.5 GRID-WIDTH)
                                   BACKGROUND))))))))))))))))))))))

(define (draw-world w)
  (draw-player (world-player w)(draw-a-lov(world-lov w))))
              
           


;;Player Image -> Image
;;draws an image at the given spot onto the given image
(check-expect (draw-player player0 BACKGROUND)
              (place-image
               PLAYER-IMG (* 7.5 GRID-WIDTH)(* 6.5 GRID-WIDTH)BACKGROUND))

(define (draw-player aplayer img)
  (place-image
   PLAYER-IMG (*(player-x aplayer)GRID-WIDTH)
   (*(player-y aplayer)GRID-WIDTH)
   img))

;;draw-a-lov:Lov -> Image
;;draws a list of  Vset onto the background
(check-expect (draw-a-lov lov0)
              (place-image VEHICLE-IMG
                           (* 2 GRID-WIDTH)
                           (* 1.5 GRID-WIDTH)
                           (place-image
                            VEHICLE-IMG
                            (* 6 GRID-WIDTH)
                            (* 1.5 GRID-WIDTH)
                            (place-image
                             VEHICLE-IMG
                             (* 10 GRID-WIDTH)
                             (* 1.5 GRID-WIDTH)
                             (place-image
                              VEHICLE-IMG
                              (* 14 GRID-WIDTH)
                              (* 1.5 GRID-WIDTH)
                              (place-image
                               VEHICLE-IMG
                               (* 2 GRID-WIDTH)
                               (* 2.5  GRID-WIDTH)
                               (place-image
                                VEHICLE-IMG
                                (* 6 GRID-WIDTH)
                                (* 2.5 GRID-WIDTH)
                                (place-image
                                 VEHICLE-IMG
                                 (* 10 GRID-WIDTH)
                                 (* 2.5 GRID-WIDTH)
                                 (place-image
                                  VEHICLE-IMG
                                  (* 14 GRID-WIDTH)
                                  (* 2.5 GRID-WIDTH)
                                  (place-image
                                   VEHICLE-IMG
                                   (* 2 GRID-WIDTH)
                                   (* 3.5 GRID-WIDTH)
                                   (place-image
                                    VEHICLE-IMG
                                    (* 6 GRID-WIDTH)
                                    (* 3.5 GRID-WIDTH)
                                    (place-image
                                     VEHICLE-IMG
                                     (* 10 GRID-WIDTH)
                                     (* 3.5 GRID-WIDTH)
                                     (place-image
                                      VEHICLE-IMG
                                      (* 14 GRID-WIDTH)
                                      (* 3.5 GRID-WIDTH)
                                      (place-image
                                       VEHICLE-IMG
                                       (* 2 GRID-WIDTH)
                                       (* 4.5 GRID-WIDTH)
                                       (place-image
                                        VEHICLE-IMG
                                        (* 6 GRID-WIDTH)
                                        (* 4.5 GRID-WIDTH)
                                        (place-image
                                         VEHICLE-IMG
                                         (* 10 GRID-WIDTH)
                                         (* 4.5 GRID-WIDTH)
                                         (place-image
                                          VEHICLE-IMG
                                          (* 14 GRID-WIDTH)
                                          (* 4.5 GRID-WIDTH)
                                          (place-image
                                           VEHICLE-IMG
                                           (* 2 GRID-WIDTH)
                                           (* 5.5 GRID-WIDTH)
                                           (place-image
                                            VEHICLE-IMG
                                            (* 6 GRID-WIDTH)
                                            (* 5.5 GRID-WIDTH)
                                            (place-image
                                             VEHICLE-IMG
                                             (* 10 GRID-WIDTH)
                                             (* 5.5 GRID-WIDTH)
                                             (place-image
                                              VEHICLE-IMG
                                              (* 14 GRID-WIDTH)
                                              (* 5.5 GRID-WIDTH)
                                              BACKGROUND)))))))))))))))))))))
(check-expect (draw-a-lov '())BACKGROUND)

(define (draw-a-lov alov)
  (cond [(empty? alov)BACKGROUND]
        [(cons? alov)(draw-a-vset (first alov)(draw-a-lov (rest alov)))]))

;;draw-a-vset: Vset Image -> Image
;;draw a set of vehicles onto a given image
(check-expect (draw-a-vset vset0-road1 BACKGROUND)
              (place-image
               VEHICLE-IMG
               (* 2 GRID-WIDTH)
               (* 1.5 GRID-WIDTH)
               (place-image
                VEHICLE-IMG
                (* 6 GRID-WIDTH)
                (* 1.5 GRID-WIDTH)
                (place-image
                 VEHICLE-IMG
                 (* 10 GRID-WIDTH)
                 (* 1.5 GRID-WIDTH)
                 (place-image
                  VEHICLE-IMG
                  (* 14 GRID-WIDTH)
                  (* 1.5 GRID-WIDTH)
                  BACKGROUND)))))
(check-expect (draw-a-vset '() BACKGROUND) BACKGROUND)
                                                
(define (draw-a-vset avset img)
  (cond
    [(empty? avset) img]
    [(cons? avset)(place-image VEHICLE-IMG
                               (*(vehicle-x (first avset))GRID-WIDTH)
                               (*(vehicle-y (first avset))GRID-WIDTH)
                               (draw-a-vset (rest avset) img))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Key-handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;change-dir: World Direction -> World
;;changes the direction of the player when a key is pressed
;;only four direction allowed "right" "left""up""down"
(check-expect (change-dir world0 "right")
              (make-world (make-player 8.5 6.5 "right")lov0))
(check-expect (change-dir world0 "left")
              (make-world (make-player 6.5 6.5 "left") lov0))
(check-expect (change-dir world0 "up")
              (make-world (make-player 7.5 5.5 "up") lov0))
(check-expect (change-dir world0 "down")
              (make-world (make-player 7.5 6.5 "down")lov0))          
(check-expect (change-dir (make-world (make-player 0.5 5.5 "left")lov0)"left")
              (make-world (make-player 0.5 5.5 "left")lov0))
(check-expect (change-dir
               (make-world (make-player 15.5 5.5 "right")lov0)"right")
              (make-world (make-player 15.5 5.5 "right")lov0))
(check-expect (change-dir (make-world (make-player 4.5 0.5 "down")lov0)"up")
              (make-world (make-player 4.5 0.5 "up")lov0))
(check-expect (change-dir (make-world (make-player 4.5 5.5 "up")lov0)"down")
              (make-world (make-player 4.5 6.5 "down")lov0))
(define (change-dir w adir)
  (cond
    [(key=? "up" adir) (make-world
                        (move-frog-up (world-player w)) (world-lov w))]
    [(key=? "down" adir)(make-world
                         (move-frog-down (world-player w))(world-lov w))]
    [(key=? "right" adir)(make-world
                          (move-frog-right (world-player w))(world-lov w))]
    [(key=? "left" adir)(make-world
                         (move-frog-left (world-player w))(world-lov w))]))
      
  
;;move-frog-up:Player  -> Player
;;direction is "up" ,when player reach the top, stay there
;;if not, move up a step and change dir  to "stop" 
(define (move-frog-up p)
  (if (= (player-y p) 0.5 )
      (make-player (player-x p)(player-y p) "up")
      (make-player (player-x p)(- (player-y p) 1) "up")))

;;move-frog-down:Player -> Player
;;direction is "down" ,when player reach the bottom, stay there
;;if not, move down a step and change dir  to "stop" 
(define (move-frog-down p)
  (if (= (player-y p) 6.5)
      (make-player (player-x p)(player-y p) "down")
      (make-player (player-x p)(+ (player-y p) 1)"down")))

;;move-frog-right:Player -> Player
;;direction is "right" ,when player reach the far right, stay there
;;if not, move right a step and change dir  to "stop" 
(define (move-frog-right p)
  (if (= (player-x p) 15.5)
      (make-player (player-x p)(player-y p) "right")
      (make-player (+ (player-x p) 1)(player-y p)"right")))

;;move-frog-left:Player -> Player
;;direction is "left" ,when player reach the far left, stay there
;;if not, move left a step and change dir  to "stop" 
(define (move-frog-left p )
  (if (= (player-x p) 0.5)
      (make-player (player-x p)(player-y p) "left")
      (make-player (- (player-x p) 1)(player-y p)"left")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Moving Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;move-world: World ->World
;;update the world at each tick
;;need more check expect????????????????????????????????????????/
(check-expect (move-world world0)
              (make-world (make-player 7.5 6.5 "up")
                          (cons
                           (cons
                            (make-vehicle 15 1.5 "right")
                            (cons
                             (make-vehicle 11 1.5 "right")
                             (cons
                              (make-vehicle 7 1.5 "right")
                              (cons
                               (make-vehicle 3 1.5 "right")
                               '()))))
                           (cons
                            (cons
                             (make-vehicle 1 2.5 "left")
                             (cons
                              (make-vehicle 5 2.5 "left")
                              (cons
                               (make-vehicle 9 2.5 "left")
                               (cons
                                (make-vehicle 13 2.5 "left")'()))))
                            (cons
                             (cons
                              (make-vehicle 15 3.5 "right")
                              (cons
                               (make-vehicle 11 3.5 "right")
                               (cons
                                (make-vehicle 7 3.5 "right")
                                (cons
                                 (make-vehicle 3 3.5 "right")'()))))
                             (cons
                              (cons
                               (make-vehicle 1 4.5 "left")
                               (cons
                                (make-vehicle 5 4.5 "left")
                                (cons
                                 (make-vehicle 9 4.5 "left")
                                 (cons
                                  (make-vehicle 13 4.5 "left")
                                  '()))))
                              (cons
                               (cons
                                (make-vehicle 15 5.5 "right")
                                (cons
                                 (make-vehicle 11 5.5 "right")
                                 (cons
                                  (make-vehicle 7 5.5 "right")
                                  (cons
                                   (make-vehicle 3 5.5 "right")
                                   '()))))
                               '())))))))

(define (move-world w)
  (make-world
   (world-player w)(move-vehicles (world-lov w))))



;;move-vehicles:Lov -> Lov
;;move all the vehicles in the given direction
(check-expect(move-vehicles
              (cons
               (cons
                (make-vehicle 14 2.5 "right")
                (cons
                 (make-vehicle 10 2.5 "right")
                 (cons
                  (make-vehicle 6 2.5 "right")
                  (cons
                   (make-vehicle 2 2.5 "right")
                   '()))))
               (cons
                (cons
                 (make-vehicle 2 3.5 "left")
                 (cons
                  (make-vehicle 6 3.5 "left")
                  (cons
                   (make-vehicle 10 3.5 "left")
                   (cons
                    (make-vehicle 14 3.5 "left")
                    '()))))
                '())))
             (cons
              (cons
               (make-vehicle 15 2.5 "right")
               (cons
                (make-vehicle 11 2.5 "right")
                (cons
                 (make-vehicle 7 2.5 "right")
                 (cons
                  (make-vehicle 3 2.5 "right")
                  '()))))
              (cons
               (cons
                (make-vehicle 1 3.5 "left")
                (cons
                 (make-vehicle 5 3.5 "left")
                 (cons
                  (make-vehicle 9 3.5 "left")
                  (cons
                   (make-vehicle 13 3.5 "left")
                   '()))))
               '())))

(check-expect(move-vehicles
              (cons
               (cons
                (make-vehicle 16 2 "right")
                (cons
                 (make-vehicle 16 2 "right")
                 (cons
                  (make-vehicle 8 2 "right")
                  (cons
                   (make-vehicle 4 2 "right")
                   '()))))
               (cons
                (cons
                 (make-vehicle 0 3 "left")
                 (cons
                  (make-vehicle 4 3 "left")
                  (cons
                   (make-vehicle 0 3 "left")
                   (cons (make-vehicle 12 3 "left")
                         '()))))
                '())))
             (cons
              (cons
               (make-vehicle 0 2 "right")
               (cons
                (make-vehicle 0 2 "right")
                (cons
                 (make-vehicle 8 2 "right")
                 (cons
                  (make-vehicle 4 2 "right")
                  '()))))
              (cons
               (cons
                (make-vehicle 16 3 "left")
                (cons
                 (make-vehicle 4 3 "left")
                 (cons
                  (make-vehicle 16 3 "left")
                  (cons
                   (make-vehicle 12 3 "left")
                   '()))))
               '())))
(check-expect (move-vehicles '())'())

(define (move-vehicles alov)
  (cond
    [(empty? alov) empty]
    [(cons? alov)
     (if(off-screen? (first alov))
        (cons (add-new (first alov))(move-vehicles (rest alov)))
        (cons (move-row (first alov ))(move-vehicles(rest alov))))]))
  
  


;;off-screen?:Vset -> Boolean
;;determin whether a vehicle is off the screen
(check-expect (off-screen? vset0-road1) #false)
(check-expect (off-screen?
               (cons (make-vehicle 16 2.5 "right")
                     (cons (make-vehicle 12 2.5 "right")
                           (cons (make-vehicle 9 2.5 "right")
                                 (cons (make-vehicle 6 2.5 "right")'())))))
              #true)
(check-expect (off-screen?
               (cons (make-vehicle 14 2.5 "right")
                     (cons (make-vehicle 11 2.5 "right")
                           (cons (make-vehicle 8 2.5 "right")
                                 (cons (make-vehicle 5 2.5 "right")'())))))
              #false)
(check-expect (off-screen?
               (cons (make-vehicle 6 2.5 "left")
                     (cons (make-vehicle 9 2.5 "left")
                           (cons (make-vehicle 12 2.5 "left")
                                 (cons (make-vehicle 15 2.5 "left")'())))))
              #false)
(check-expect (off-screen?
               (cons (make-vehicle 0 2.5 "left")
                     (cons (make-vehicle 3 2.5 "left")
                           (cons (make-vehicle 6 2.5 "left")
                                 (cons (make-vehicle 9 2.5 "left")'())))))
              #true)
(check-expect (off-screen?
               (cons (make-vehicle 14 2.5 "right")
                     (cons (make-vehicle 16 2.5 "right")
                           (cons (make-vehicle 8 2.5 "right")
                                 (cons (make-vehicle 5 2.5 "right")'())))))
              #true)
(check-expect (off-screen?
               (cons (make-vehicle 14 2.5 "left")
                     (cons (make-vehicle 0 2.5 "left")
                           (cons (make-vehicle 8 2.5 "left")
                                 (cons (make-vehicle 5 2.5 "left")'())))))
              #true)
(check-expect (off-screen?
               (cons (make-vehicle 14 2.5 "left")
                     (cons (make-vehicle 10 2.5 "left")
                           (cons (make-vehicle 8 2.5 "left")
                                 (cons (make-vehicle 5 2.5 "left")'())))))
              #false)
(check-expect (off-screen? '())
              #false)

(define (off-screen? avset )
  (cond
    [(empty? avset)#false]
    [(cons? avset)
     (or(or (and(string=? (vehicle-dir (first avset)) "left")
                (= (vehicle-x (first avset)) 0))
            (off-screen? (rest avset)))
        (or(and(string=? (vehicle-dir (first avset)) "right")
               (= (vehicle-x (first avset)) 16))
           (off-screen? (rest avset))))]))

;;add-new: Vset -> Vset
;;add new vehicle if any vehicle is off-screen
;; need to be shortened ????????????????????????????????????????????/

(check-expect (add-new
               (cons (make-vehicle 16 2 "right")
                     (cons (make-vehicle 12 2 "right")
                           (cons (make-vehicle 8 2 "right")
                                 (cons (make-vehicle 4 2 "right")'())))))
             
              (cons (make-vehicle 0 2 "right")
                    (cons (make-vehicle 12 2 "right")
                          (cons (make-vehicle 8 2 "right")
                                (cons (make-vehicle 4 2 "right")'())))))
(check-expect (add-new
               (cons (make-vehicle 0 2 "left")
                     (cons (make-vehicle 4 2 "left")
                           (cons (make-vehicle 8 2 "left")
                                 (cons (make-vehicle 12 2 "left")'())))))
              
              (cons (make-vehicle 16 2 "left")
                    (cons (make-vehicle 4 2 "left")
                          (cons (make-vehicle 8 2 "left")
                                (cons (make-vehicle 12 2 "left")'())))))
(check-expect (add-new '())'())
(check-expect (add-new
               (cons (make-vehicle 14 2 "right")
                     (cons (make-vehicle 10 2 "right")
                           (cons (make-vehicle 6 2 "right")
                                 (cons (make-vehicle 2 2 "right")'())))))

              (cons (make-vehicle 14 2 "right")
                    (cons (make-vehicle 10 2 "right")
                          (cons (make-vehicle 6 2 "right")
                                (cons (make-vehicle 2 2 "right")'())))))
               
(check-expect (add-new
               (cons (make-vehicle 14 2 "right")
                     (cons (make-vehicle 16 2 "right")
                           (cons (make-vehicle 6 2 "right")
                                 (cons (make-vehicle 2 2 "right")'())))))

              (cons (make-vehicle 14 2 "right")
                    (cons (make-vehicle 0 2 "right")
                          (cons (make-vehicle 6 2 "right")
                                (cons (make-vehicle 2 2 "right")'())))))

(check-expect (add-new
               (cons (make-vehicle 4 2 "left")
                     (cons (make-vehicle 0 2 "left")
                           (cons (make-vehicle 8 2 "left")
                                 (cons (make-vehicle 12 2 "left")'())))))
              
              (cons (make-vehicle 4 2 "left")
                    (cons (make-vehicle 16 2 "left")
                          (cons (make-vehicle 8 2 "left")
                                (cons (make-vehicle 12 2 "left")'())))))
                              
           
(define (add-new avset)
  (cond
    [(empty? avset) empty]
    [(and (string=? (vehicle-dir (first avset))"left")
          (= (vehicle-x (first avset)) 0))
     (cons (make-vehicle 16 (vehicle-y (first avset))"left")
           (add-new(rest avset)))]
    [(and (string=? (vehicle-dir (first avset))"right")
          (= (vehicle-x (first avset)) 16))
     (cons (make-vehicle 0 (vehicle-y (first avset))"right")
           (add-new (rest avset)))]
    [else (cons(first avset)(add-new (rest avset)))]))



;;move-row: Vset -> Vset
;;move all vehicles in a row one step towards their given direction
(check-expect (move-row vset0-road2)
              (cons (make-vehicle 1 2.5 "left")
                    (cons (make-vehicle 5 2.5 "left")
                          (cons (make-vehicle 9 2.5 "left")
                                (cons (make-vehicle 13 2.5 "left")'())))))
(check-expect (move-row vset0-road3)
              (cons (make-vehicle 15 3.5 "right")
                    (cons (make-vehicle 11 3.5 "right")
                          (cons (make-vehicle 7 3.5 "right")
                                (cons (make-vehicle 3 3.5 "right")'())))))
              
(define (move-row avset)
  (cond
    [(string=? (vehicle-dir(first avset)) "left")
     (move-row-left avset)]
    [(string=? (vehicle-dir(first avset))"right")
     (move-row-right avset)]))

;;move-row-left:Vset-> Vset
;;move vehicles in a row one step to the left
(check-expect (move-row-left vset0-road2)
              (cons (make-vehicle 1 2.5 "left")
                    (cons (make-vehicle 5 2.5 "left")
                          (cons (make-vehicle 9 2.5 "left")
                                (cons (make-vehicle 13 2.5 "left")'())))))
(define (move-row-left avset)
  (cond
    [(empty? avset)empty]
    [(cons? avset)
     (cons (make-vehicle (- (vehicle-x (first avset)) 1)
                         (vehicle-y (first avset))
                         "left")(move-row-left (rest avset)))]))

;;move-row-right:Vset-> Vset
;;move vehicles in a row one step to the right
(check-expect (move-row-right vset0-road3)
              (cons (make-vehicle 15 3.5 "right")
                    (cons (make-vehicle 11 3.5 "right")
                          (cons (make-vehicle 7 3.5 "right")
                                (cons (make-vehicle 3 3.5 "right")'())))))
(define (move-row-right avset)
  (cond
    [(empty? avset)empty]
    [(cons? avset)
     (cons (make-vehicle (+ (vehicle-x (first avset)) 1)
                         (vehicle-y (first avset))
                         "right")(move-row-right (rest avset)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Collision Detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;collision? World -> Boolean
;;did the player collide with any of the vehicles 
(check-expect (collision? world0) #false)
(check-expect (collision? (make-world (make-player 6 5.5 "right")lov0 ))#true)
(check-expect (collision? (make-world player0 '()))#false)

(define (collision? w)
  (cond
    [(empty? (world-lov w)) #false]
    [(cons? (world-lov w))
     (or(hit-within-row? (world-player w)(first (world-lov w)))
        (collision? (make-world (world-player w)
                                (rest (world-lov w)))))]))
   
;;hit-within-row?: Player,Vset -> Boolean
;;was the player hit within a specific row of vehicles?
(check-expect (hit-within-row? player0 vset0-road5) #false)
(check-expect (hit-within-row? player0 '())#false)
(check-expect (hit-within-row? (make-player 2 5.5 "right")vset0-road5)#true)

(define (hit-within-row? player vset)
  (cond
    [(empty? vset) #false]
    [(cons? vset)
     (or (hit? player (first vset))
         (hit-within-row? player (rest vset)))]))
      

;;hit?:Player,Vehicle -> Boolean
;;was the player hit by the vehicle?
(check-expect (hit? player0 (make-vehicle 7 6.5 "right"))
              #true)
(check-expect (hit? player0 (make-vehicle 3 4.5 "left"))
              #false)
(define (hit? aplayer av)
  (and (= (player-y aplayer)
          (vehicle-y av))
       (in-range? (*(player-x aplayer)GRID-WIDTH)
                  (*(vehicle-x av)GRID-WIDTH)
                  (+ (/ (image-width PLAYER-IMG)2)
                     (/ (image-width VEHICLE-IMG)2)))))
;;in-range?: Number, Number,Number -> Boolean
;;is n1 within range of n2?
(check-expect (in-range? 10 17 2) #false)
(check-expect (in-range? 13 14 2)#true)
(define (in-range? n1 n2 range)
  (and (< n1 (+ n2 range))
       (> n1 (- n2 range))))

;;reach-end? World -> Boolean
;;did the player reach the other side of the road?
(check-expect (reach-end? world0) #false)
(check-expect (reach-end? (make-world (make-player 6 0.5 "right")lov0 ))#true)
(check-expect (reach-end? (make-world player0 '()))#false)
(define (reach-end? w)
  (= (player-y (world-player w)) 0.5))
;; show-gameover: World -> Image
;; show the game-over image when the player hits a vehicle

(check-expect (show-gameover (make-world (make-player 2.5 2.5 "left")lov0))
              (place-image
               (text "Game Over !" 60 'purple)
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (place-image
                PLAYER-IMG
                (* 2.5 GRID-WIDTH)
                (* 2.5 GRID-WIDTH)
                (place-image
                 VEHICLE-IMG
                 (* 2 GRID-WIDTH)
                 (* 1.5 GRID-WIDTH)
                 (place-image
                  VEHICLE-IMG
                  (* 6 GRID-WIDTH)
                  (* 1.5 GRID-WIDTH)
                  (place-image
                   VEHICLE-IMG
                   (* 10 GRID-WIDTH)
                   (* 1.5 GRID-WIDTH)
                   (place-image
                    VEHICLE-IMG
                    (* 14 GRID-WIDTH)
                    (* 1.5 GRID-WIDTH)
                    (place-image
                     VEHICLE-IMG
                     (* 2 GRID-WIDTH)
                     (* 2.5  GRID-WIDTH)
                     (place-image
                      VEHICLE-IMG
                      (* 6 GRID-WIDTH)
                      (* 2.5 GRID-WIDTH)
                      (place-image
                       VEHICLE-IMG
                       (* 10 GRID-WIDTH)
                       (* 2.5 GRID-WIDTH)
                       (place-image
                        VEHICLE-IMG
                        (* 14 GRID-WIDTH)
                        (* 2.5 GRID-WIDTH)
                        (place-image
                         VEHICLE-IMG
                         (* 2 GRID-WIDTH)
                         (* 3.5 GRID-WIDTH)
                         (place-image
                          VEHICLE-IMG
                          (* 6 GRID-WIDTH)
                          (* 3.5 GRID-WIDTH)
                          (place-image
                           VEHICLE-IMG
                           (* 10 GRID-WIDTH)
                           (* 3.5 GRID-WIDTH)
                           (place-image
                            VEHICLE-IMG
                            (* 14 GRID-WIDTH)
                            (* 3.5 GRID-WIDTH)
                            (place-image
                             VEHICLE-IMG
                             (* 2 GRID-WIDTH)
                             (* 4.5 GRID-WIDTH)
                             (place-image
                              VEHICLE-IMG
                              (* 6 GRID-WIDTH)
                              (* 4.5 GRID-WIDTH)
                              (place-image
                               VEHICLE-IMG
                               (* 10 GRID-WIDTH)
                               (* 4.5 GRID-WIDTH)
                               (place-image
                                VEHICLE-IMG
                                (* 14 GRID-WIDTH)
                                (* 4.5 GRID-WIDTH)
                                (place-image
                                 VEHICLE-IMG
                                 (* 2 GRID-WIDTH)
                                 (* 5.5 GRID-WIDTH)
                                 (place-image
                                  VEHICLE-IMG
                                  (* 6 GRID-WIDTH)
                                  (* 5.5 GRID-WIDTH)
                                  (place-image
                                   VEHICLE-IMG
                                   (* 10 GRID-WIDTH)
                                   (* 5.5 GRID-WIDTH)
                                   (place-image
                                    VEHICLE-IMG
                                    (* 14 GRID-WIDTH)
                                    (* 5.5 GRID-WIDTH)
                                    BACKGROUND)))))))))))))))))))))))
                 
(define (show-gameover w)
  (place-image
   (text "Game Over !" 60 'purple)
   (/ WIDTH 2)
   (/ HEIGHT 2)
   (draw-world w)))



;; show-youwin: World -> Image
;; show the youwin image when the player reaches the other end of the road

(check-expect (show-youwin (make-world (make-player 2.5 0.5 "left")lov0))
              (place-image
               (text "You Win !" 60 'purple)
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (place-image
                PLAYER-IMG
                (* 2.5 GRID-WIDTH)
                (* 0.5 GRID-WIDTH)
                (place-image
                 VEHICLE-IMG
                 (* 2 GRID-WIDTH)
                 (* 1.5 GRID-WIDTH)
                 (place-image
                  VEHICLE-IMG
                  (* 6 GRID-WIDTH)
                  (* 1.5 GRID-WIDTH)
                  (place-image
                   VEHICLE-IMG
                   (* 10 GRID-WIDTH)
                   (* 1.5 GRID-WIDTH)
                   (place-image
                    VEHICLE-IMG
                    (* 14 GRID-WIDTH)
                    (* 1.5 GRID-WIDTH)
                    (place-image
                     VEHICLE-IMG
                     (* 2 GRID-WIDTH)
                     (* 2.5  GRID-WIDTH)
                     (place-image
                      VEHICLE-IMG
                      (* 6 GRID-WIDTH)
                      (* 2.5 GRID-WIDTH)
                      (place-image
                       VEHICLE-IMG
                       (* 10 GRID-WIDTH)
                       (* 2.5 GRID-WIDTH)
                       (place-image
                        VEHICLE-IMG
                        (* 14 GRID-WIDTH)
                        (* 2.5 GRID-WIDTH)
                        (place-image
                         VEHICLE-IMG
                         (* 2 GRID-WIDTH)
                         (* 3.5 GRID-WIDTH)
                         (place-image
                          VEHICLE-IMG
                          (* 6 GRID-WIDTH)
                          (* 3.5 GRID-WIDTH)
                          (place-image
                           VEHICLE-IMG
                           (* 10 GRID-WIDTH)
                           (* 3.5 GRID-WIDTH)
                           (place-image
                            VEHICLE-IMG
                            (* 14 GRID-WIDTH)
                            (* 3.5 GRID-WIDTH)
                            (place-image
                             VEHICLE-IMG
                             (* 2 GRID-WIDTH)
                             (* 4.5 GRID-WIDTH)
                             (place-image
                              VEHICLE-IMG
                              (* 6 GRID-WIDTH)
                              (* 4.5 GRID-WIDTH)
                              (place-image
                               VEHICLE-IMG
                               (* 10 GRID-WIDTH)
                               (* 4.5 GRID-WIDTH)
                               (place-image
                                VEHICLE-IMG
                                (* 14 GRID-WIDTH)
                                (* 4.5 GRID-WIDTH)
                                (place-image
                                 VEHICLE-IMG
                                 (* 2 GRID-WIDTH)
                                 (* 5.5 GRID-WIDTH)
                                 (place-image
                                  VEHICLE-IMG
                                  (* 6 GRID-WIDTH)
                                  (* 5.5 GRID-WIDTH)
                                  (place-image
                                   VEHICLE-IMG
                                   (* 10 GRID-WIDTH)
                                   (* 5.5 GRID-WIDTH)
                                   (place-image
                                    VEHICLE-IMG
                                    (* 14 GRID-WIDTH)
                                    (* 5.5 GRID-WIDTH)
                                    BACKGROUND)))))))))))))))))))))))
                 
(define (show-youwin w)
  (place-image
   (text "You Win !" 60 'purple)
   (/ WIDTH 2)
   (/ HEIGHT 2)
   (draw-world w)))


(main world0)









