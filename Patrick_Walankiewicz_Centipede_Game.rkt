;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Patrick_Walankiewicz_Centipede_Game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Patrick Walankiewicz

(require 2htdp/image)
(require 2htdp/universe)


(define GRID-WIDTH 25)
(define GRID-HEIGHT 30)
(define CELL-SIZE 15)
(define BACKGROUND (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))

(define PLAYER (square CELL-SIZE 'solid 'blue))
(define BULLET (rectangle 3 8 'solid 'black))
(define CENTIPEDE-CELL (circle (/ CELL-SIZE 2) 'solid 'green))
(define TONGUE (square 5 'solid 'red))
(define LEFT-HEAD (overlay/align "left" "middle" TONGUE CENTIPEDE-CELL))
(define RIGHT-HEAD (overlay/align "right" "middle" TONGUE CENTIPEDE-CELL))

(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'Salmon)
(define MUSHROOM-2-C 'OrangeRed)
(define MUSHROOM-3-C 'Red)
(define MUSHROOM-4-C 'DarkRed)

(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))

;A LOC is one of:
;empty
;(cons (make-centipede seg) LOC)

;A world is one of:
;(make-world centipede player bullet)
;Interpretation: A world stores the conditions of                    
;the centipede, player and bullet.
(define-struct world [centipede player bullet mushroom])

;A LOS is one of:
;empty
;(cons (make-seg String Make-Posn) LOS)

;A centipede is a (make-centipede (cons (make-seg String Make-Posn) LOS)
;Interpretation: A centipede is a list of segments, and keeps track
;of their directions a positions
(define-struct centipede [seg])

;A seg is a (make-seg String Make-Posn String)
;Interpretation: Each (make-seg String Make-Posn) is an
;individual piece of the centipede that has a horizontal direction,
;position and a vertical direction
(define-struct seg [direction pos ydir])

;A player is a (make-player Number Number)
;Interpretation: A player stores the x position
;and the y position of the player throughout
;the program
(define-struct player [x y])

;A bullet is one of:
;(make-bullet (cons Number (cons Number empty)))
;(make-bullet empty)
;Interpretation: A bullet stores a number inicating its speed
;list of two numbers, the first
;number is the x position of the bullet and the second number is
;the y position of the bullet. If no bullet exists then a bullet
;contains an empty list
(define-struct bullet [speed pos])

;A LOM is one of:
;empty
;(cons (make-mushroom Number Make-Posn (cons LOM))

;A mushroom is one of:
;(make-mushroom Number LOM)
;Interpretation: A mushroom stores
;a number that indicates how many times the
;mushroom has been hit by a bullet and the position
;of the mushroom
(define-struct mushroom [phase pos])

(define (main w0 m0)
  (big-bang (make-world
             (cons (make-centipede (convert-centipede (range 0 w0 1))) empty)
             (make-player 13 
                          29)
             (make-bullet 1 empty)
             (generate-mushrooms (range 0 m0 1)))
            [to-draw render]
            [on-key change]
            [on-tick update .03]
            [stop-when dead? end-screen]))

;Function (change w ke)
;[world, Key] -> [world]
;This function checks if the player pressed any keys
;and returns the new state of the world based on what
;key was pressed

(define (change w ke)
  (cond
    [(and (string=? ke "right")
          (< (player-x (world-player w)) 24))
     (make-world (world-centipede w)
                 (make-player (+ (player-x (world-player w)) 1)
                              (player-y (world-player w)))
                 (world-bullet w)
                 (world-mushroom w))]
    [(and (string=? ke "left")
          (> (player-x (world-player w)) 0))
     (make-world (world-centipede w)
                 (make-player (- (player-x (world-player w)) 1)
                              (player-y (world-player w)))
                 (world-bullet w)
                 (world-mushroom w))]          
    [(and (string=? ke " ")
          (bullet-exists? w))
     (make-world (world-centipede w)
                 (world-player w)
                 (make-bullet 1 (cons (player-x (world-player w))
                                      (cons (- (player-y (world-player w)) 1) empty)))
                 (world-mushroom w))]
    [else w]))

(check-expect (change (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (cons 20 (cons 28 empty)))
                                  (list (make-mushroom 'Salmon (make-posn 4 4))))
                               
                      "right")
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                          (make-player 21 29)
                          (make-bullet 1 (cons 20 (cons 28 empty)))
                          (list (make-mushroom 'Salmon (make-posn 4 4)))))

(check-expect (change (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (cons 20 (cons 28 empty)))
                                  (list (make-mushroom 'Salmon (make-posn 4 4))))
                      "left")
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                          (make-player 19 29)
                          (make-bullet 1 (cons 20 (cons 28 empty)))
                          (list (make-mushroom 'Salmon (make-posn 4 4)))))

(check-expect (change (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 empty)
                                  (list (make-mushroom 'Salmon (make-posn 4 4))))
                      " ")
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                          (make-player 20 29)
                          (make-bullet 1 (cons 20 (cons 28 empty)))
                          (list (make-mushroom 'Salmon (make-posn 4 4)))))

(check-expect (change (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (list 20 28))
                                  (list (make-mushroom 'Salmon (make-posn 4 4))))
                      " ")
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                          (make-player 20 29)
                          (make-bullet 1 (cons 20 (cons 28 empty)))
                          (list (make-mushroom 'Salmon (make-posn 4 4)))))

;Function (bullet-exists? w)
;[world] -> [Boolean]
;This is a helper function for (change w ke)
;This function takes in a world and checks if a bullet
;exists. If a bullet does not exist the function returns true.
;If the bullet does exist the function returns false.

(define (bullet-exists? w)
  (cond
    [(empty? (bullet-pos (world-bullet w)))
     #true]
    [(and (cons? (bullet-pos (world-bullet w)))
          (< (second (bullet-pos (world-bullet w))) 0))
     #true]
    [else #false]))

(check-expect (bullet-exists? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (list 20 28))
                                  (list (make-mushroom 'Salmon (make-posn 4 4)))))
              #false)

(check-expect (bullet-exists? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (list 20 -5))
                                  (list (make-mushroom 'Salmon (make-posn 4 4)))))
              #true)

(check-expect (bullet-exists? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                        (make-seg "right" (make-posn 4 5) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 empty)
                                  (list (make-mushroom 'Salmon (make-posn 4 4)))))
              #true)

;Function: (update w0)
;[world] -> [world]
;This is the on-tick function that takes in
;a world and returns a new world based on its
;conditions

(define (update w0)
  (cond
    [(mush-collision? w0) (make-world
                           (update-centipede-list w0)
                           (world-player w0)
                           (make-bullet 1 empty)
                           (change-mushroom (mush-collision-where? w0)
                                            (world-mushroom w0)))] 
    [(collision? w0)
     (make-world (cut-centipede (world-centipede w0)
                                (collision-where? w0)
                                (which-segment? w0))
                 (world-player w0)
                 (make-bullet 1 empty)
                 (cons (make-mushroom 1 (make-posn
                                         (first (bullet-pos (world-bullet w0)))
                                         (second (bullet-pos (world-bullet w0)))))
                       (world-mushroom w0)))]
    [(and (not (= (bullet-speed (world-bullet w0)) 4))
          (not (bullet-exists? w0)))
     (make-world (world-centipede w0)
                 (world-player w0)
                 (make-bullet (+ 1 (bullet-speed (world-bullet w0)))
                              (cons (first (bullet-pos (world-bullet w0)))
                                    (cons (- (second (bullet-pos (world-bullet w0))) 1) empty)))
                 (world-mushroom w0))]
    [(not (= (bullet-speed (world-bullet w0)) 4))
     (make-world (world-centipede w0)
                 (world-player w0)
                 (make-bullet (+ 1 (bullet-speed (world-bullet w0)))
                              empty)
                 (world-mushroom w0))]
    [else
     (make-world (update-centipede-list w0)
                 (world-player w0)
                 (make-bullet 1 (bullet-pos (world-bullet w0)))
                 (world-mushroom w0))]))

(check-expect (update (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 empty)
                                  (list (make-mushroom 1 (make-posn 4 4)))))
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                (make-seg "right" (make-posn 19 20) "down"))))
                          (make-player 20 29)
                          (make-bullet 2 empty)
                          (list (make-mushroom 1 (make-posn 4 4)))))

(check-expect (update (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (cons 19 (cons 20 empty)))
                                  (list (make-mushroom 1 (make-posn 4 4)))))
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down"))))
                                (make-player 20 29)
                                (make-bullet 1 '())
                                (list (make-mushroom 1 (make-posn 19 20))
                                      (make-mushroom 1 (make-posn 4 4)))))

(check-expect (update (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (cons 20 (cons 29 empty)))
                                  (list (make-mushroom 1 (make-posn 4 4)))))
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))                     
                          (make-player 20 29)
                          (make-bullet 2 (cons 20 (cons 28 empty)))
                          (list (make-mushroom 1 (make-posn 4 4)))))

(check-expect (update (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 4 (cons 20 (cons 29 empty)))
                                  (list (make-mushroom 1 (make-posn 4 4)))))
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 21 20) "down")
                                                        (make-seg "right" (make-posn 20 20) "down"))))                     
                          (make-player 20 29)
                          (make-bullet 1 (cons 20 (cons 29 empty)))
                          (list (make-mushroom 1 (make-posn 4 4)))))

(check-expect (update (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 29)
                                  (make-bullet 1 (cons 20 (cons 29 empty)))
                                  (list (make-mushroom 1 (make-posn 20 29)))))
              (make-world (list (make-centipede (list (make-seg "right" (make-posn 21 20) "down")
                                                        (make-seg "right" (make-posn 20 20) "down"))))                     
                          (make-player 20 29)
                          (make-bullet 1 empty)
                          (list (make-mushroom 2 (make-posn 20 29)))))

;Function (mush-collision? w)
;[world] -> [boolean]
;Function checks the world to see whether a bullet
;and a mushroom have the same coordinates

(define (mush-collision? w)
  (cond
    [(empty? (world-mushroom w)) #false]
    [(empty? (bullet-pos (world-bullet w))) #false]
    [(and (= (posn-x (mushroom-pos (first (world-mushroom w))))
             (first (bullet-pos (world-bullet w))))
          (= (posn-y (mushroom-pos (first (world-mushroom w))))
             (second (bullet-pos (world-bullet w)))))
     #true]
    [else (mush-collision? (make-world
                            (world-centipede w)
                            (world-player w)
                            (world-bullet w)
                            (rest (world-mushroom w))))]))

(check-expect (mush-collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 20)
                                  (make-bullet 1 (cons 20 (cons 29 empty)))
                                  (list (make-mushroom 1 (make-posn 20 29)))))
              #true)

(check-expect (mush-collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 20)
                                  (make-bullet 1 (cons 20 (cons 20 empty)))
                                  (list (make-mushroom 1 (make-posn 20 29)))))
              #false)

(check-expect (mush-collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 20)
                                  (make-bullet 1 empty)
                                  (list (make-mushroom 1 (make-posn 20 29)))))
              #false)

;Function (mush-collision-where? w)
;[world] -> [mushroom]
;After a collision is confirmed by (mush-collision? w),
;this function takes in a world and outputs the mushroom
;that the bullet collided with

(define (mush-collision-where? w)
  (cond
    [(and (= (posn-x (mushroom-pos (first (world-mushroom w))))
             (first (bullet-pos (world-bullet w))))
          (= (posn-y (mushroom-pos (first (world-mushroom w))))
             (second (bullet-pos (world-bullet w)))))
     (first (world-mushroom w))]
    [else (mush-collision-where? (make-world
                                  (world-centipede w)
                                  (world-player w)
                                  (world-bullet w)
                                  (rest (world-mushroom w))))]))

(check-expect (mush-collision-where? (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 20)
                                  (make-bullet 1 (list 20 29))
                                  (list (make-mushroom 1 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40)))))
              (make-mushroom 1 (make-posn 20 29)))

(check-expect (mush-collision-where? (make-world (list (make-centipede (list (make-seg "right" (make-posn 20 20) "down")
                                                        (make-seg "right" (make-posn 19 20) "down"))))
                                  (make-player 20 20)
                                  (make-bullet 1 (list 20 40))
                                  (list (make-mushroom 1 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40)))))
              (make-mushroom 1 (make-posn 20 40)))

;Function: (change-mushroom where LOM)
;[mushroom, LOM] -> [LOM]
;Function takes in a list of mushrooms
;and the mushroom that was hit by a bullet
;and updates the list of mushrooms with the
;new mushroom properties

(define (change-mushroom where LOM)
  (cond
    [(empty? LOM) empty]
    [(and (= (mushroom-phase (first LOM)) 4)
          (equal? where (first LOM)))
     (change-mushroom where (rest LOM))]
    [(equal? where (first LOM))
     (cons (make-mushroom (+ (mushroom-phase (first LOM)) 1)
                          (mushroom-pos (first LOM)))
           (rest LOM))]
    [else (cons (first LOM) (change-mushroom where (rest LOM)))]))

(check-expect (change-mushroom (make-mushroom 1 (make-posn 20 29))
                                  (list (make-mushroom 1 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40))))
              (list (make-mushroom 2 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40))))

(check-expect (change-mushroom (make-mushroom 1 (make-posn 20 40))
                                  (list (make-mushroom 1 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40))))
              (list (make-mushroom 1 (make-posn 20 29))
                                        (make-mushroom 2 (make-posn 20 40))))

(check-expect (change-mushroom (make-mushroom 4 (make-posn 20 29))
                                  (list (make-mushroom 4 (make-posn 20 29))
                                        (make-mushroom 1 (make-posn 20 40))))
              (list (make-mushroom 1 (make-posn 20 40))))

;Function (collison? w)
;[world] -> [boolean]
;Function takes in a world and checks
;if a bullet exists, and returns true if
;the bullet collides with a centipede. 

(define (collision? w)
  (cond
    [(empty? (world-centipede w)) #false]
    [(empty? (centipede-seg (first (world-centipede w))))
     (collision? (make-world
                  (rest (world-centipede w))
                  (world-player w)
                  (world-bullet w)
                  (world-mushroom w)))]
    [(empty? (bullet-pos (world-bullet w))) #false]
    [(and (= (posn-x (seg-pos (first (centipede-seg (first (world-centipede w))))))
             (first (bullet-pos (world-bullet w))))
          (= (posn-y (seg-pos (first (centipede-seg (first (world-centipede w)))))) 
             (second (bullet-pos (world-bullet w)))))
     
     #true]
    [else (collision? (make-world
                       (cons (make-centipede (rest (centipede-seg (first (world-centipede w)))))
                             (rest (world-centipede w)))
                       (world-player w)
                       (world-bullet w)
                       (world-mushroom w)))]))

(check-expect (collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                  (make-seg "right" (make-posn 4 5) "down")
                                                                  (make-seg "right" (make-posn 3 5) "down"))))
                                      (make-player 20 29)
                                      (make-bullet 1 (cons 4 (cons 5 empty)))
                                      (make-mushroom 1 empty)))
              #true)

(check-expect (collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                  (make-seg "right" (make-posn 4 5) "down")
                                                                  (make-seg "right" (make-posn 3 5) "down")))
                                            (make-centipede (list (make-seg "right" (make-posn 5 10) "down")
                                                                  (make-seg "right" (make-posn 4 10) "down")
                                                                  (make-seg "right" (make-posn 3 10) "down"))))
                                      (make-player 20 29)
                                      (make-bullet 1 (cons 4 (cons 10 empty)))
                                      (make-mushroom 1 empty)))
              #true)

(check-expect (collision? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                  (make-seg "right" (make-posn 4 5) "down")
                                                                  (make-seg "right" (make-posn 3 5) "down")))
                                            (make-centipede (list (make-seg "right" (make-posn 5 10) "down")
                                                                  (make-seg "right" (make-posn 4 10) "down")
                                                                  (make-seg "right" (make-posn 3 10) "down"))))
                                      (make-player 20 29)
                                      (make-bullet 1 (cons 4 (cons 15 empty)))
                                      (make-mushroom 1 empty)))
              #false)

;Function (centipede-analysis w bullet)
;[centipede, bullet] -> [boolean]
;This is a helper function for (collision-where? w)
;Function takes in a centipede and checks to see
;if the bullet has collided with any of the segments
;in that centipede

(define (centipede-analysis w bullet)
  (cond
    [(empty? (centipede-seg w)) #false]
    [(and (= (posn-x (seg-pos (first (centipede-seg w))))
             (first (bullet-pos bullet)))
          (= (posn-y (seg-pos (first (centipede-seg w)))) 
             (second (bullet-pos bullet))))
     #true]
    [else (centipede-analysis (make-centipede (rest (centipede-seg w))) bullet)]))

(check-expect (centipede-analysis (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                        (make-seg "right" (make-posn 4 5) "down")
                                                                        (make-seg "right" (make-posn 3 5) "down")))
                                  (make-bullet 1 (list 4 5)))
              #true)

(check-expect (centipede-analysis (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                        (make-seg "right" (make-posn 4 5) "down")
                                                                        (make-seg "right" (make-posn 3 5) "down")))
                                  (make-bullet 1 (list 4 10)))
              #false)

;Function (collision-where? w)
;[world] -> [centipede]
;Function takes in a world, checks which
;centipede the bullet collided with and
;returns that centipede

(define (collision-where? w)
  (cond
    [(centipede-analysis (first (world-centipede w)) (world-bullet w))
     (first (world-centipede w))]
    [else (collision-where? (make-world
                             (rest (world-centipede w))
                             (world-player w)
                             (world-bullet w)
                             (world-mushroom w)))]))

(check-expect (collision-where? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                        (make-seg "right" (make-posn 4 5) "down")
                                                                        (make-seg "right" (make-posn 3 5) "down"))))
                                            (make-player 200 200)
                                            (make-bullet 1 (cons 4 (cons 5 empty)))
                                            (make-mushroom 'red empty)))
              (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                    (make-seg "right" (make-posn 4 5) "down")
                                    (make-seg "right" (make-posn 3 5) "down"))))

(check-expect (collision-where? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                        (make-seg "right" (make-posn 4 5) "down")
                                                                        (make-seg "right" (make-posn 3 5) "down")))
                                                  (make-centipede (list (make-seg "right" (make-posn 5 10) "down")
                                                                        (make-seg "right" (make-posn 4 10) "down")
                                                                        (make-seg "right" (make-posn 3 10) "down"))))
                                            (make-player 200 200)
                                            (make-bullet 1 (cons 4 (cons 10 empty)))
                                            (make-mushroom 'red empty)))
              (make-centipede (list (make-seg "right" (make-posn 5 10) "down")
                                    (make-seg "right" (make-posn 4 10) "down")
                                    (make-seg "right" (make-posn 3 10) "down"))))

;Function (which-segment? w)
;[world] -> [seg]
;Function takes in a world and returns the segment
;from the centipede that got hit with the bullet

(define (which-segment? w)
  (cond
    [(empty? (centipede-seg (first (world-centipede w))))
     (which-segment? (make-world
                      (rest (world-centipede w))
                      (world-player w)
                      (world-bullet w)
                      (world-mushroom w)))]
    [(and (= (posn-x (seg-pos (first (centipede-seg (first (world-centipede w))))))
             (first (bullet-pos (world-bullet w))))
          (= (posn-y (seg-pos (first (centipede-seg (first (world-centipede w)))))) 
             (second (bullet-pos (world-bullet w)))))
     
     (first (centipede-seg (first (world-centipede w))))]
    [else (which-segment? (make-world
                           (cons (make-centipede (rest (centipede-seg (first (world-centipede w)))))
                                 (rest (world-centipede w)))
                           (world-player w)
                           (world-bullet w)
                           (world-mushroom w)))]))

(check-expect (which-segment? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))))
                                          (make-player 200 200)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 'red empty)))
              (make-seg "right" (make-posn 4 5) "down"))

(check-expect (which-segment? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down")))
                                                (make-centipede (list (make-seg "right" (make-posn 5 10) "down")
                                                                      (make-seg "right" (make-posn 4 10) "down")
                                                                      (make-seg "right" (make-posn 3 10) "down"))))
                                          (make-player 200 200)
                                          (make-bullet 1 (cons 4 (cons 10 empty)))
                                          (make-mushroom 'red empty)))
              (make-seg "right" (make-posn 4 10) "down"))

;Function: (dead-check? w0)
;[world] -> [boolean]
;Function takes in a world and checks
;whether the first segment in each centipede
;has touched the player

(define (dead-check? w0)
  (cond
    [(empty? (world-centipede w0))
     #false]
    [(empty? (centipede-seg (first (world-centipede w0))))
     (dead-check? (make-world
                   (rest (world-centipede w0))
                   (world-player w0)
                   (world-bullet w0)
                   (world-mushroom w0)))]
    [(and (= (posn-x (seg-pos (first (centipede-seg (first (world-centipede w0))))))
             (player-x (world-player w0)))
          (= (posn-y (seg-pos (first (centipede-seg (first (world-centipede w0))))))
             (player-y (world-player w0))))
     #true]
    [else (dead-check? (make-world
                        (rest (world-centipede w0))
                        (world-player w0)
                        (world-bullet w0)
                        (world-mushroom w0)))]))

(check-expect (dead-check? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))))
                                          (make-player 5 5)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #true)

(check-expect (dead-check? (make-world (list (make-centipede empty)
                                             (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down")))
                                             (make-centipede (list (make-seg "right" (make-posn 5 10) "down")))
                                             (make-centipede (list (make-seg "right" (make-posn 6 10) "down"))))
                                          (make-player 6 10)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #true)

(check-expect (dead-check? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))))
                                          (make-player 5 10)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #false)

;Function: (dead-centipede? w)
;[LOC] -> [boolean]
;This is a helper function for (dead? w0)
;This function takes in a list of centipedes
;and determines whether a centipede exists

(define (dead-centipede? w)
  (cond
    [(empty? w) #true]
    [(empty? (centipede-seg (first w)))
     (dead-centipede? (rest w))]
    [else #false]))

(check-expect (dead-centipede? (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down")))))
              #false)

(check-expect (dead-centipede? (list (make-centipede empty) (make-centipede empty)))
              #true)

;Function: (dead? w0)
;[world] -> [boolean]
;This is the stop-when function.
;Function takes in a world and checks if
;the centipede has reached the player or if
;the centipede doesnt exist anymore.
;The function returns true one of these cases
;is true

(define (dead? w0)
  (cond
    [(dead-centipede? (world-centipede w0))
     #true]
    [(dead-check? w0)
     #true]
    [else
     #false]))


(check-expect (dead? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))))
                                          (make-player 5 5)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #true)

(check-expect (dead? (make-world (list (make-centipede empty))
                                          (make-player 5 5)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #true)

(check-expect (dead? (make-world (list (make-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))))
                                          (make-player 6 5)
                                          (make-bullet 1 (cons 4 (cons 5 empty)))
                                          (make-mushroom 1 empty)))
              #false)


;Function: (update-centipede-list w0)
;[LOC] -> [LOC]
;Function takes in a list of centipedes and returns
;a new list of centipedes each with a new list of segments 

(define (update-centipede-list w0)
  (cond
    [(empty? (world-centipede w0)) empty]
    [(cons? (world-centipede w0))
     (cons (make-centipede (update-centipede (centipede-seg (first (world-centipede w0))) (world-mushroom w0)))
           (update-centipede-list (make-world (rest (world-centipede w0))
                                              (world-player w0)
                                              (world-bullet w0)
                                              (world-mushroom w0))))]))

;Function: (update-centipede w0)
;[LOS] -> [LOS]
;Function takes in a list of segments and checks
;each segment for both of it's directions, whether it hit a mushroom
;and whether it has hit the walls and makes a new
;centipede accordingly

(define (update-centipede w0 m0)
  (cond
    [(empty? w0) empty]
    [(and (and (= (posn-y (seg-pos (first w0))) 0)
               (= (posn-x (seg-pos (first w0))) 0))
          (string=? (seg-direction (first w0)) "left"))
     (cons (make-seg
            "right"
            (make-posn (posn-x (seg-pos (first w0)))
                       (+ (posn-y (seg-pos (first w0))) 1))
            "down")
           (update-centipede (rest w0) m0))]
    [(and (and (= (posn-y (seg-pos (first w0))) 0)
               (= (posn-x (seg-pos (first w0))) 24))
          (string=? (seg-direction (first w0)) "right"))
     (cons (make-seg
            "left"
            (make-posn (posn-x (seg-pos (first w0)))
                       (+ (posn-y (seg-pos (first w0))) 1))
            "down")
           (update-centipede (rest w0) m0))]
    [(and (and (= (posn-y (seg-pos (first w0))) 29)
               (= (posn-x (seg-pos (first w0))) 0))
          (string=? (seg-direction (first w0)) "left"))
     (cons (make-seg
            "right"
            (make-posn (posn-x (seg-pos (first w0)))
                       (- (posn-y (seg-pos (first w0))) 1))
            "up")
           (update-centipede (rest w0) m0))]
    [(and (and (= (posn-y (seg-pos (first w0))) 29)
               (= (posn-x (seg-pos (first w0))) 24))
          (string=? (seg-direction (first w0)) "right"))
     (cons (make-seg
            "left"
            (make-posn (posn-x (seg-pos (first w0)))
                       (- (posn-y (seg-pos (first w0))) 1))
            "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "right")
          (run-mushroom? (first w0) m0)
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "left" (make-posn (posn-x (seg-pos (first w0)))
                                       (- (posn-y (seg-pos (first w0))) 1))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "left")
          (run-mushroom? (first w0) m0)
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "right" (make-posn (posn-x (seg-pos (first w0)))
                                        (- (posn-y (seg-pos (first w0))) 1))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "right")
          (= (posn-x (seg-pos (first w0)))
             24)
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "left" (make-posn (posn-x (seg-pos (first w0)))
                                       (- (posn-y (seg-pos (first w0))) 1))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "left")
          (= (posn-x (seg-pos (first w0)))
             0)
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "right" (make-posn (posn-x (seg-pos (first w0)))
                                        (- (posn-y (seg-pos (first w0))) 1))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "right")
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "right" (make-posn (+ (posn-x (seg-pos (first w0))) 1)
                                        (posn-y (seg-pos (first w0))))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "left")
          (string=? (seg-ydir (first w0)) "up"))
     (cons (make-seg "left" (make-posn (- (posn-x (seg-pos (first w0))) 1)
                                       (posn-y (seg-pos (first w0))))
                     "up")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "right")
          (run-mushroom? (first w0) m0)
          (string=? (seg-ydir (first w0)) "down"))
     (cons (make-seg "left" (make-posn (posn-x (seg-pos (first w0)))
                                       (+ (posn-y (seg-pos (first w0))) 1))
                     "down")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "left")
          (run-mushroom? (first w0) m0)
          (string=? (seg-ydir (first w0)) "down"))
     (cons (make-seg "right" (make-posn (posn-x (seg-pos (first w0)))
                                        (+ (posn-y (seg-pos (first w0))) 1))
                     "down")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "right")
          (= (posn-x (seg-pos (first w0)))
             24)
          (string=? (seg-ydir (first w0)) "down"))
     (cons (make-seg "left" (make-posn (posn-x (seg-pos (first w0)))
                                       (+ (posn-y (seg-pos (first w0))) 1))
                     "down")
           (update-centipede (rest w0) m0))]
    [(and (string=? (seg-direction (first w0)) "left")
          (= (posn-x (seg-pos (first w0)))
             0)
          (string=? (seg-ydir (first w0)) "down"))
     (cons (make-seg "right" (make-posn (posn-x (seg-pos (first w0)))
                                        (+ (posn-y (seg-pos (first w0))) 1))
                     "down")
           (update-centipede (rest w0) m0))]
    [(string=? (seg-direction (first w0)) "right")
     (cons (make-seg "right" (make-posn (+ (posn-x (seg-pos (first w0))) 1)
                                        (posn-y (seg-pos (first w0))))
                     "down")
           (update-centipede (rest w0) m0))]
    [(string=? (seg-direction (first w0)) "left")
     (cons (make-seg "left" (make-posn (- (posn-x (seg-pos (first w0))) 1)
                                       (posn-y (seg-pos (first w0))))
                     "down")
           (update-centipede (rest w0) m0))]))

(check-expect (update-centipede (list (make-seg "left" (make-posn 5 5) "down")
                                                                      (make-seg "left" (make-posn 6 5) "down")
                                                                      (make-seg "left" (make-posn 7 5) "down"))
                                          empty)
              (list (make-seg "left" (make-posn 4 5) "down")
                                                                      (make-seg "left" (make-posn 5 5) "down")
                                                                      (make-seg "left" (make-posn 6 5) "down")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")
                                                                      (make-seg "right" (make-posn 3 5) "down"))
                                          empty)
              (list (make-seg "right" (make-posn 6 5) "down")
                                                                      (make-seg "right" (make-posn 5 5) "down")
                                                                      (make-seg "right" (make-posn 4 5) "down")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 24 5) "down")
                                                                      (make-seg "right" (make-posn 23 5) "down")
                                                                      (make-seg "right" (make-posn 22 5) "down"))
                                          empty)
              (list (make-seg "left" (make-posn 24 6) "down")
                                                                      (make-seg "right" (make-posn 24 5) "down")
                                                                      (make-seg "right" (make-posn 23 5) "down")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 0 5) "down")
                                                                      (make-seg "left" (make-posn 1 5) "down")
                                                                      (make-seg "left" (make-posn 2 5) "down"))
                                          empty)
              (list (make-seg "right" (make-posn 0 6) "down")
                                                                      (make-seg "left" (make-posn 0 5) "down")
                                                                      (make-seg "left" (make-posn 1 5) "down")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 5 5) "down"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 4 5))))
              (list (make-seg "right" (make-posn 5 6) "down")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 5 5) "down"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "left" (make-posn 5 6) "down")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 5 5) "up")
                                                                      (make-seg "left" (make-posn 6 5) "up")
                                                                      (make-seg "left" (make-posn 7 5) "up"))
                                          empty)
              (list (make-seg "left" (make-posn 4 5) "up")
                                                                      (make-seg "left" (make-posn 5 5) "up")
                                                                      (make-seg "left" (make-posn 6 5) "up")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 5 5) "up")
                                                                      (make-seg "right" (make-posn 4 5) "up")
                                                                      (make-seg "right" (make-posn 3 5) "up"))
                                          empty)
              (list (make-seg "right" (make-posn 6 5) "up")
                                                                      (make-seg "right" (make-posn 5 5) "up")
                                                                      (make-seg "right" (make-posn 4 5) "up")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 24 5) "up")
                                                                      (make-seg "right" (make-posn 23 5) "up")
                                                                      (make-seg "right" (make-posn 22 5) "up"))
                                          empty)
              (list (make-seg "left" (make-posn 24 4) "up")
                                                                      (make-seg "right" (make-posn 24 5) "up")
                                                                      (make-seg "right" (make-posn 23 5) "up")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 0 5) "up")
                                                                      (make-seg "left" (make-posn 1 5) "up")
                                                                      (make-seg "left" (make-posn 2 5) "up"))
                                          empty)
              (list (make-seg "right" (make-posn 0 4) "up")
                                                                      (make-seg "left" (make-posn 0 5) "up")
                                                                      (make-seg "left" (make-posn 1 5) "up")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 5 5) "up"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 4 5))))
              (list (make-seg "right" (make-posn 5 4) "up")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 5 5) "up"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "left" (make-posn 5 4) "up")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 24 0) "up"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "left" (make-posn 24 1) "down")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 0 0) "up"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "right" (make-posn 0 1) "down")))

(check-expect (update-centipede (list (make-seg "right" (make-posn 24 29) "down"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "left" (make-posn 24 28) "up")))

(check-expect (update-centipede (list (make-seg "left" (make-posn 0 29) "down"))
                                                                      
                                          (list (make-mushroom 1 (make-posn 6 5))))
              (list (make-seg "right" (make-posn 0 28) "up")))

;Function (run-mushroom w0 m0)
;[seg, LOM] -> [boolean]
;Function takes in a segment and checks
;whether it has ran into a mushroom

(define (run-mushroom? w0 m0)
  (cond
    [(empty? m0) #false]
    [(or (and (= (posn-x (seg-pos w0))
                 (- (posn-x (mushroom-pos (first m0))) 1))
              (string=? (seg-direction w0) "right")
              (= (posn-y (seg-pos w0))
                 (posn-y (mushroom-pos (first m0)))))
         
         (and (= (posn-x (seg-pos w0))
                 (+ (posn-x (mushroom-pos (first m0))) 1))
              (string=? (seg-direction w0) "left")
              (= (posn-y (seg-pos w0))
                 (posn-y (mushroom-pos (first m0))))))
     #true]
    [else (run-mushroom? w0 (rest m0))]))

(check-expect (run-mushroom?
               (make-seg "right" (make-posn 5 5) "down")
               (list (make-mushroom 1 (make-posn 6 5))))
              #true)

(check-expect (run-mushroom?
               (make-seg "left" (make-posn 5 5) "down")
               (list (make-mushroom 1 (make-posn 4 5))))
              #true)

(check-expect (run-mushroom?
               (make-seg "right" (make-posn 5 5) "down")
               (list (make-mushroom 1 (make-posn 10 5))))
              #false)

;Function (cut-centipede w centipede segment)
;[LOC, centipede, seg] -> [LOC]
;Function takes in a list of centipdes,
;the centipede that got hit by the bullet
;and the segment that got hit by the bullet
;and returns a new list of centipedes 

(define (cut-centipede w centipede segment)
  (cond
    [(empty? w) empty]
    [(and (equal? (first w) centipede)
          (= (length (split-centipede centipede segment)) 1))
     (cons (first (split-centipede centipede segment))
           (cut-centipede (rest w) centipede segment))]
    [(equal? (first w) centipede)
     (cons (first (split-centipede centipede segment))
           (cons (second (split-centipede centipede segment))
                 (cut-centipede (rest w) centipede segment)))]
    [else (cons (first w)
                (cut-centipede (rest w) centipede segment))]))

(check-expect (cut-centipede (list (make-centipede
                                    (list (make-seg "right" (make-posn 11 4) "down")
                                          (make-seg "right" (make-posn 10 4) "down")
                                          (make-seg "right" (make-posn 9 4) "down")))
                                   (make-centipede
                                    (list (make-seg "right" (make-posn 11 10) "down")
                                          (make-seg "right" (make-posn 10 10) "down")
                                          (make-seg "right" (make-posn 9 10) "down"))))
                             (make-centipede
                              (list (make-seg "right" (make-posn 11 10) "down")
                                    (make-seg "right" (make-posn 10 10) "down")
                                    (make-seg "right" (make-posn 9 10) "down")))
                             (make-seg "right" (make-posn 10 10) "down"))
              (list (make-centipede
                     (list (make-seg "right" (make-posn 11 4) "down")
                           (make-seg "right" (make-posn 10 4) "down")
                           (make-seg "right" (make-posn 9 4) "down")))
                    (make-centipede (list (make-seg "right" (make-posn 11 10) "down")))
                    (make-centipede (list (make-seg "right" (make-posn 9 10) "down")))))

(check-expect (cut-centipede (list (make-centipede
                                    (list (make-seg "right" (make-posn 11 4) "down")
                                          (make-seg "right" (make-posn 10 4) "down")
                                          (make-seg "right" (make-posn 9 4) "down")))
                                   (make-centipede
                                    (list (make-seg "right" (make-posn 11 10) "down"))))
                             (make-centipede
                              (list (make-seg "right" (make-posn 11 10) "down")))
                             (make-seg "right" (make-posn 11 10) "down"))
              (list (make-centipede
                     (list (make-seg "right" (make-posn 11 4) "down")
                           (make-seg "right" (make-posn 10 4) "down")
                           (make-seg "right" (make-posn 9 4) "down")))
                    (make-centipede empty)))

;Function (split-centipede centipede segment)
;[centipede, seg] -> [LOC]
;Function takes in the centipede that got hit
;by the bullet and the segment that got hit by the
;bullet and creates a list of two centipedes, splitting
;the inital centipede at the given segment

(define (split-centipede centipede segment)
  (cond
    [(empty? (first-half (centipede-seg centipede) segment))
     (cons (make-centipede (second-half (centipede-seg centipede) segment 1)) empty)]
    [(empty? (second-half (centipede-seg centipede) segment 1))
     (cons (make-centipede (first-half (centipede-seg centipede) segment)) empty)]
    [else (cons (make-centipede (first-half (centipede-seg centipede) segment))
                (cons (make-centipede (second-half (centipede-seg centipede) segment 1))
                      empty))]))


(check-expect (split-centipede (make-centipede
                                (list (make-seg "right" (make-posn 11 4) "down")
                                      (make-seg "right" (make-posn 10 4) "down") 
                                      (make-seg "right" (make-posn 9 4) "down")))
                               (make-seg "right" (make-posn 11 4) "down"))
              (cons (make-centipede (list (make-seg "right" (make-posn 10 4) "down")
                                          (make-seg "right" (make-posn 9 4) "down")))
                    empty))

(check-expect (split-centipede (make-centipede
                                (list (make-seg "right" (make-posn 11 4)  "down")
                                      (make-seg "right" (make-posn 10 4)  "down")
                                      (make-seg "right" (make-posn 9 4) "down")))
                               (make-seg "right" (make-posn 10 4) "down"))
              (list (make-centipede (list (make-seg "right" (make-posn 11 4)  "down")))
                    (make-centipede (list (make-seg "right" (make-posn 9 4)  "down")))))

(check-expect (split-centipede (make-centipede
                                (list (make-seg "right" (make-posn 11 4) "down")
                                      (make-seg "right" (make-posn 10 4) "down")
                                      (make-seg "right" (make-posn 9 4) "down")))
                               (make-seg "right" (make-posn 9 4) "down"))
              (list (make-centipede (list (make-seg "right" (make-posn 11 4) "down")
                                          (make-seg "right" (make-posn 10 4) "down")))))
(check-expect (split-centipede (make-centipede
                                (list (make-seg "right" (make-posn 11 4)  "down")))
                               
                               (make-seg "right" (make-posn 11 4)  "down"))
              (list (make-centipede empty)))

;Function (first-half LOS segment)
;[LOS, seg] -> [LOS]
;Function takes in a list of segments
;and the segment that was hit by the bullet
;and returns the first half of the centipede
;that was hit 

(define (first-half LOS segment)
  (cond
    [(empty? LOS) empty]
    [(equal? (first LOS) segment)
     empty]
    [else
     (cons (first LOS) (first-half (rest LOS) segment))]))

(check-expect (first-half
               (list (make-seg "right" (make-posn 5 5) "down")
               (make-seg "right" (make-posn 4 5) "down")
               (make-seg "right" (make-posn 3 5) "down"))
               (make-seg "right" (make-posn 4 5) "down"))
              (list (make-seg "right" (make-posn 5 5) "down")))

(check-expect (first-half
               empty
               (make-seg "right" (make-posn 5 5) "down"))
              empty)
               

;Function (first-half LOS segment)
;[LOS, seg, Number] -> [LOS]
;Function takes in a list of segments
;and the segment that was hit by the bullet
;and returns the second half of the centipede
;that was hit

(define (second-half LOS segment counter)
  (cond
    [(empty? LOS) empty]
    [(= counter 2)
     (cons (first LOS) (second-half (rest LOS) segment 2))]
    [(equal? (first LOS) segment)
     (second-half (rest LOS) segment 2)]
    [else
     (second-half (rest LOS) segment 1)]))

(check-expect (second-half
               (list (make-seg "right" (make-posn 5 5) "down")
               (make-seg "right" (make-posn 4 5) "down")
               (make-seg "right" (make-posn 3 5) "down"))
               (make-seg "right" (make-posn 4 5) "down")
               1)
              (list (make-seg "right" (make-posn 3 5) "down")))

(check-expect (second-half
               empty
               (make-seg "right" (make-posn 4 5) "down")
               1)
              empty)

;Function (render w0)
;This is the to-draw function
;[world] -> [image]
;Function takes in a world
;and returns an image based
;on the conditions in the world

(define (render w0)
  (cond
    [(bullet-exists? w0)
     (draw-mushroom w0)]
    [else
     (place-image/grid BULLET (first (bullet-pos (world-bullet w0)))
                       (second (bullet-pos (world-bullet w0)))
                       (draw-mushroom w0))]))

;Function: (draw-right-head w0) 
;[world] -> [image]
;This function is a helper function for (render w0)
;They take in a world and return an image of the centipede and
;The player based on the conditions of the world

(define (draw-right-head w0)
  (place-image/grid PLAYER (player-x (world-player w0))
                    (player-y (world-player w0))
                    (place-image/grid RIGHT-HEAD (posn-x (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                                      (posn-y (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                                      (draw-body (make-world
                                                  (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (world-mushroom w0))))))

;Function: (draw-left-head w0) 
;[world] -> [image]
;This function is a helper function for (render w0)
;They take in a world and return an image of the centipede and
;The player based on the conditions of the world

(define (draw-left-head w0)
  (place-image/grid PLAYER (player-x (world-player w0))
                    (player-y (world-player w0))
                    (place-image/grid LEFT-HEAD (posn-x (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                                      (posn-y (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                                      (draw-body (make-world
                                                  (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (world-mushroom w0))))))

;Function:(draw-body w0)
;[LOS] -> [image]
;Function takes in a LOS and draws
;each individual segment onto an empty scene


(define (draw-body w0)
  (cond
    [(empty? (centipede-seg (first (world-centipede w0))))
     (draw-centipedes (make-world
                       (rest (world-centipede w0))
                       (world-player w0)
                       (world-bullet w0)
                       (world-mushroom w0)))]
    [(cons? (world-centipede w0))
     (place-image/grid CENTIPEDE-CELL (posn-x (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                       (posn-y (seg-pos (first (centipede-seg (first (world-centipede w0))))))
                       (draw-body (make-world
                                   (cons (make-centipede (rest (centipede-seg (first (world-centipede w0))))) (rest (world-centipede w0))) 
                                   (world-player w0)
                                   (world-bullet w0)
                                   (world-mushroom w0))))]))

(define (draw-mushroom w0)
  (cond
    [(empty? (world-mushroom w0))
     (draw-centipedes w0)] 
    [(and (cons? (world-mushroom w0))
          (= (mushroom-phase (first (world-mushroom w0))) 1))
     (place-image/grid (circle MUSHROOM-RADIUS 'solid MUSHROOM-1-C)
                       (posn-x (mushroom-pos (first (world-mushroom w0))))
                       (posn-y (mushroom-pos (first (world-mushroom w0))))
                       (draw-mushroom (make-world (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (rest (world-mushroom w0)))))]
    [(and (cons? (world-mushroom w0))
          (= (mushroom-phase (first (world-mushroom w0))) 2))
     (place-image/grid (circle MUSHROOM-RADIUS 'solid MUSHROOM-2-C)
                       (posn-x (mushroom-pos (first (world-mushroom w0))))
                       (posn-y (mushroom-pos (first (world-mushroom w0))))
                       (draw-mushroom (make-world (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (rest (world-mushroom w0)))))]
    [(and (cons? (world-mushroom w0))
          (= (mushroom-phase (first (world-mushroom w0))) 3))
     (place-image/grid (circle MUSHROOM-RADIUS 'solid MUSHROOM-3-C)
                       (posn-x (mushroom-pos (first (world-mushroom w0))))
                       (posn-y (mushroom-pos (first (world-mushroom w0))))
                       (draw-mushroom (make-world (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (rest (world-mushroom w0)))))]
    [(and (cons? (world-mushroom w0))
          (= (mushroom-phase (first (world-mushroom w0))) 4))
     (place-image/grid (circle MUSHROOM-RADIUS 'solid MUSHROOM-4-C)
                       (posn-x (mushroom-pos (first (world-mushroom w0))))
                       (posn-y (mushroom-pos (first (world-mushroom w0))))
                       (draw-mushroom (make-world (world-centipede w0)
                                                  (world-player w0)
                                                  (world-bullet w0)
                                                  (rest (world-mushroom w0)))))]))

(define (draw-centipedes w0)
  (cond
    [(empty? (world-centipede w0))
     BACKGROUND]
    [(empty? (centipede-seg (first (world-centipede w0))))
     (draw-centipedes (make-world
                       (rest (world-centipede w0))
                       (world-player w0)
                       (world-bullet w0)
                       (world-mushroom w0)))]
    [(string=? (seg-direction (first (centipede-seg (first (world-centipede w0)))))
               "right")
     (draw-right-head w0)]
    [(string=? (seg-direction (first (centipede-seg (first (world-centipede w0)))))
               "left")
     (draw-left-head w0)]))


;Function (centipede-conversion w0)
;[list] -> [LOS]
;Function takes in the range from (convert-centipede w0)
;and then creates a LOS based on that range

(define (convert-centipede w0)
  (cond
    [(empty? w0) empty]
    [(cons? w0)
     (cons (make-seg "right" (make-posn (- 20 (first w0)) 0) "down")
           (convert-centipede (rest w0)))]))

                   

;Function (end-screen w0)
;[world] -> [image]
;Function takes in a world and returns the last image
;of the program when stop-when returns #true

(define (end-screen w0)
  (cond
    [(dead-centipede? (world-centipede w0))
     (place-image WINNER (/ (* GRID-WIDTH CELL-SIZE) 2)
                  (/ (* GRID-HEIGHT CELL-SIZE) 2)
                  BACKGROUND)]
    [else
     (place-image LOSER (/ (* GRID-WIDTH CELL-SIZE) 2)
                  (/ (* GRID-HEIGHT CELL-SIZE) 2)
                  BACKGROUND)]))


(define (generate-mushrooms m0)
  (cond
    [(empty? m0) empty]
    [(cons? m0)
     (cons (make-mushroom 1 (make-posn (random 25)
                                       (filter-numbers (random 29))))
           (generate-mushrooms (rest m0)))]))

(define (filter-numbers number)
  (cond
    [(= number 0)
     (filter-numbers (random 29))]
    [else
     number]))

(define (place-image/grid img grid-x grid-y bg)
  (place-image img
               (+ (* grid-x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* grid-y CELL-SIZE) (/ CELL-SIZE 2))
               bg))

(main 20 60)
















