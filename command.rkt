;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname command) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A space-war simulation

; requires
(require 2htdp/universe)
(require 2htdp/image)

; physical constants
(define RADIUS 20)
(define WIDTH 20)
(define SIZE 20)
(define SCENE-SIZE (* WIDTH SIZE))
(define MAX (+ 1 (- SCENE-SIZE SIZE)))
(define START-MSG (text "WELCOME TO COMMAND!" SIZE 'black))
(define INSTRUCTION-MSG1 (text "MOVE TANK WITH THE DIRECTION KEYS"
                               16 'lightgray))
(define INSTRUCTION-MSG2 (text "PRESS SPACE TO FIRE A MISSILE"
                               16 'lightgray))
(define INSTRUCTION-MSG3 (text "PRESS SPACE TO START"
                               16 'white))
(define LAST-MSG (text "GAME OVER" SIZE 'black))

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE 'darkgray))
(define UFO (circle RADIUS 'solid 'silver))
(define TANK (square SIZE 'solid 'white))
(define MISSILE
  (overlay
   (rectangle (- (/ SIZE 4) 2) (- (/ SIZE 2) 2) 'solid 'white)
   (rectangle (/ SIZE 4) (/ SIZE 2) 'solid 'gray)))
(define SURFACE (rectangle (- SCENE-SIZE 1) (- SIZE 1) 'solid 'lightgray))
(define SCENE
  (place-image SURFACE (/ SCENE-SIZE 2) (- SCENE-SIZE (/ SIZE 2)) MT))

; structures
(define-struct world [tank ufo])
(define-struct tank [position missile])
(define-struct ufo [position direction])

; A World is a structure:
; (make-world Tank Ufo)
; Interpretation:
; a (make-world t u) is the state of game at a given time. The tank t
; and ufo u are represented by a world

(define WORLD0 (make-world (make-tank (make-posn 0 0) '())
                           (make-ufo (make-posn 1 1) "right")))
(define WORLD1 (make-world (make-tank (make-posn 100 100) '())
                           (make-ufo (make-posn 10 10) "right")))
(define WORLD2 (make-world (make-tank
                            (make-posn
                             (/ SCENE-SIZE 2)
                             (- SCENE-SIZE (+ SIZE (/ SIZE 2))))
                            '())
                           (make-ufo (make-posn (/ SCENE-SIZE 2) SIZE) "")))

; A Tank is a structure:
; (make-tank Posn Missile)
; A Missile is one of:
; - '()
; - (cons Posn Missile)
; Interpretation:
; A (make-tank p m) is the player controlled armour at a point p, x/y
; SIZE away from the top-left corner of the game screen.
; A missile m is a list of posns that represents shots from a tank
; x/y SIZE from the top-left corner of the game-screen.

(define TANK0 (make-tank (make-posn 9 9) '()))
(define TANK1 (make-tank (make-posn 9 9) (cons (make-posn 1 1) '())))

; A Ufo is a structure:
; (make-ufo Posn String)
; Interpretation:
; a (make-ufo p s) is an enemy craft at a point p x/y SIZE from the
; top-left corner of the game screen and with a string s "left" or
; "right" that dictates the next direction of movement.

(define UFO0 (make-ufo (make-posn 0 0) ""))
(define UFO1 (make-ufo (make-posn 1 1) "right"))
(define UFO2 (make-ufo (make-posn 0 1) "left"))

; Functions

; World -> Image
; consumes a world and renders the state to the screen

(check-expect
 (render-world
  (make-world
   (make-tank (make-posn 9 9) '()) (make-ufo (make-posn 0 0) "")))
 (place-image TANK 9 9
              (place-image UFO 0 0 SCENE)))

(define (fn-render-world w)
  (cond
    [(empty? (tank-missile (world-tank w)))
     (... ... (posn-x (tank-position (world-tank w)))
          (posn-y (tank-position (world-tank w)))
          (... ... (posn-x (ufo-position (world-ufo w)))
               (posn-y (ufo-position (world-ufo w))) SCENE))]
    [else (... ... (posn-x (... (tank-missile (world-tank w))))
               (posn-y (... (tank-missile (world-tank w))))
               (fn-render-world
                (make-world
                 (make-tank (tank-position (world-tank w))
                            (rest (tank-missile (world-tank w))))
                 (world-ufo w))))]))

(define (render-world w)
  (cond
    [(start? w) (start w)]
    [(empty? (tank-missile (world-tank w)))
     (place-image TANK (posn-x (tank-position (world-tank w)))
                  (posn-y (tank-position (world-tank w)))
                  (place-image UFO (posn-x (ufo-position (world-ufo w)))
                               (posn-y (ufo-position (world-ufo w))) SCENE))]
    [else (place-image
           MISSILE
           (posn-x (first (tank-missile (world-tank w))))
           (posn-y  (first (tank-missile (world-tank w))))
           (render-world
            (make-world
             (make-tank (tank-position (world-tank w))
                        (rest (tank-missile (world-tank w))))
             (world-ufo w))))]))

; World Key -> World
; consumes a world and a key and outputs a new world

(check-expect (control (make-world (make-tank (make-posn 0 0) '())
                                   (make-ufo (make-posn 1 1) "")) "")
              (make-world (make-tank (make-posn 0 0) '())
                          (make-ufo (make-posn 1 1) "")))

(check-expect (control (make-world (make-tank (make-posn 0 0) '())
                                   (make-ufo (make-posn 1 1) "")) " ")
              (make-world (make-tank (make-posn 0 0)
                                     (cons (make-posn 0 350) '()))
                          (make-ufo (make-posn 1 1) "")))

(check-expect (control (make-world (make-tank (make-posn 0 0) '())
                                   (make-ufo (make-posn 1 1) "")) "right")
              (make-world (make-tank (make-posn 20 0) '())
                          (make-ufo (make-posn 1 1) "")))

(check-expect (control (make-world (make-tank (make-posn 20 0) '())
                                   (make-ufo (make-posn 1 1) "")) "left")
              (make-world (make-tank (make-posn 0 0) '())
                          (make-ufo (make-posn 1 1) "")))

(define (fn-control w key)
  (cond
    [(string=? key "right")
     (make-world
      (make-tank (make-posn (... (posn-x (tank-position (world-tank w))))
                            (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key "left")
     (make-world
      (make-tank (make-posn (... (posn-x (tank-position (world-tank w))))
                            (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key " ")
     (make-world
      (make-tank (tank-position (world-tank w))
                 (... (make-posn 0 0) (tank-missile (world-tank w))))
      (world-ufo w))]
    [else w]))

(define (control w key)
  (cond
    [(and (start? w) (string=? "left" key)) w]
    [(and (start? w) (string=? "right" key)) w]
    [(and (start? w) (string=? " " key))
     (ufo-random-x
      (make-world
       (world-tank w)
       (make-ufo (ufo-position (world-ufo w))
                 (ufo-direction (world-ufo (direction-random w))))))]
    [(string=? key "right")
     (cond
       [(tank-boundary?
         (make-world
          (make-tank (make-posn (+ (posn-x (tank-position (world-tank w))) SIZE)
                                (posn-y (tank-position (world-tank w))))
                     (tank-missile (world-tank w)))
          (world-ufo w)))
        w]
       [else
        (make-world
         (make-tank (make-posn (+ (posn-x (tank-position (world-tank w))) SIZE)
                               (posn-y (tank-position (world-tank w))))
                    (tank-missile (world-tank w)))
         (world-ufo w))])]
    [(string=? key "left")
     (cond
       [(tank-boundary?
         (make-world
          (make-tank (make-posn (- (posn-x (tank-position (world-tank w))) SIZE)
                                (posn-y (tank-position (world-tank w))))
                     (tank-missile (world-tank w)))
          (world-ufo w)))
        w]
       [else 
        (make-world
         (make-tank (make-posn (- (posn-x (tank-position (world-tank w))) SIZE)
                               (posn-y (tank-position (world-tank w))))
                    (tank-missile (world-tank w)))
         (world-ufo w))])]
    [(string=? key " ")
     (make-world
      (make-tank (tank-position (world-tank w))
                 (cons (make-posn (posn-x (tank-position (world-tank w)))
                                  (- SCENE-SIZE (+ (* SIZE 2) (/ SIZE 2))))
                       (tank-missile (world-tank w))))
      (world-ufo w))]
    [else w]))

; World -> Boolean
; consumes a world w and outputs true if the tank has exceeded the boundary
; of the game screen

(check-expect (tank-boundary? (make-world (make-tank (make-posn 0 10) '())
                                          (make-ufo (make-posn 20 20) "")))
              #false)

(check-expect (tank-boundary? (make-world (make-tank (make-posn -1 10) '())
                                          (make-ufo (make-posn 20 20) "")))
              #true)

(check-expect (tank-boundary? (make-world (make-tank (make-posn 400 10) '())
                                          (make-ufo (make-posn 20 20) "")))
              #false)

(check-expect (tank-boundary? (make-world (make-tank (make-posn 401 10) '())
                                          (make-ufo (make-posn 20 20) "")))
              #true)

(define (fn-tank-boundary? w)
  (cond
    [(< (posn-x (tank-position (world-tank w))) 0)
     ...]
    [(> (posn-x (tank-position (world-tank w))) SCENE-SIZE)
     ...]
    [else
     ...]))

(define (tank-boundary? w)
  (cond
    [(< (posn-x (tank-position (world-tank w))) 0)
     #true]
    [(> (posn-x (tank-position (world-tank w))) SCENE-SIZE)
     #true]
    [else
     #false]))

; World > World
; consumes a world and outputs a new world each tick every rate seconds

;(check-expect (tock (make-world (make-tank (make-posn 0 0) '())
;                                (make-ufo (make-posn 10 10) "")))
;              (make-world
;               (make-tank (make-posn 0 0) '())
;               (make-ufo (make-posn 10 (+ 10 (/ SIZE 2))) "")))
;
;(check-expect (tock (make-world (make-tank (make-posn 0 0)
;                                           (cons (make-posn 20 40) '()))
;                                (make-ufo (make-posn 10 10) "")))
;              (make-world
;               (make-tank (make-posn 0 0)
;                          (cons (make-posn 20 (- 40 SIZE)) '()))
;               (make-ufo (make-posn 10 (+ 10 (/ SIZE 2))) "")))
;
;(check-expect (tock (make-world (make-tank (make-posn 0 0)
;                                           (cons (make-posn 20 40) '()))
;                                (make-ufo (make-posn 10 10) "right")))
;              (make-world
;               (make-tank (make-posn 0 0)
;                          (cons (make-posn 20 (- 40 SIZE)) '()))
;               (make-ufo
;                (make-posn (+ 10 SIZE) (+ 10 (/ SIZE 2))) "right")))
;
;(check-expect (tock (make-world (make-tank (make-posn 0 0)
;                                           (cons (make-posn 20 40) '()))
;                                (make-ufo (make-posn 40 40) "left")))
;              (make-world
;               (make-tank (make-posn 0 0)
;                          (cons (make-posn 20 (- 40 SIZE)) '()))
;               (make-ufo
;                (make-posn (- 40 SIZE) (+ 40 (/ SIZE 2))) "left")))


(define (fn-tock w)
  (make-world
   (make-tank (tank-position (world-tank w))
              (cond
                [else (if (empty? (tank-missile (world-tank w)))
                          (tank-missile (world-tank w))
                          (... w))]))
   (make-ufo
    (make-posn
     (cond
       [else
        (if (string=? "right" (ufo-direction (world-ufo w)))
            (... (posn-x (ufo-position (world-ufo w))) ...)
            (... (posn-x (ufo-position (world-ufo w))) ...))])
     (... (posn-y (ufo-position (world-ufo w))) ...))
    (ufo-direction (world-ufo w)))))
                     

(define (tock w)
  (cond
    [(start? w) w]
    [else
     (make-world
      (make-tank (tank-position (world-tank w))
                 (cond
                   [else (if (empty? (tank-missile (world-tank w)))
                             (tank-missile (world-tank w))
                             (update-missile (tank-missile (world-tank w))))]))
      (make-ufo
       (make-posn
        (cond
          [(ufo-exceed-x? w) (posn-x (ufo-position (world-ufo w)))]
          [(string=? "right" (ufo-direction (world-ufo w)))
           (+ (posn-x (ufo-position (world-ufo w))) SIZE)]
          [(string=? "left" (ufo-direction (world-ufo w)))
           (- (posn-x (ufo-position (world-ufo w))) SIZE)]
          [else (posn-x (ufo-position (world-ufo w)))])
        (+ (posn-y (ufo-position (world-ufo w))) (/ SIZE 2)))
       (ufo-direction (world-ufo (direction-random w)))))]))

; Missile -> Missile
; consumes a missile m and updates the list of missiles by SIZE

(check-expect (update-missile '()) '())

(check-expect
 (update-missile (cons (make-posn 20 100) '()))
 (cons (make-posn 20 (- 100 SIZE)) '()))

(check-expect
 (update-missile (cons (make-posn 20 100)
                       (cons (make-posn 20 160) '())))
 (cons (make-posn 20 (- 100 SIZE))
       (cons (make-posn 20 (- 160 SIZE)) '())))


(define (fn-update-missile m)
  (cond
    [(empty? m) ...]
    [else (... (make-posn (posn-x (first m))
                          (... posn-y (first m)) ...)
               (fn-update-missile (rest m)))]))

(define (update-missile m)
  (cond
    [(empty? m) '()]
    [else (cons (make-posn (posn-x (first m))
                           (- (posn-y (first m)) SIZE))
                (update-missile (rest m)))]))

; World -> Boolean
; consumes a world w and outputs true if the condition
; for the final scene is met

(check-expect (last-world?
               (make-world (make-tank (make-posn 40 40) '())
                           (make-ufo (make-posn 60 60) "")))
              #false)

(check-expect (last-world?
               (make-world (make-tank (make-posn 40 40) '())
                           (make-ufo
                            (make-posn 10 (- SCENE-SIZE (- RADIUS 1))) "")))
              #true)

(check-expect (last-world?
               (make-world (make-tank (make-posn 40 40)
                                      (cons (make-posn 100 100) '()))
                           (make-ufo (make-posn 100 100) "")))
              #true)

(define (fn-last-world? w)
  (cond
    [else (if (or (>= (posn-y (ufo-position (world-ufo w)))
                      (- SCENE-SIZE (* RADIUS 2)))
                  (ufo-missile-collision? w))
              ...
              ...)]))

(define (last-world? w)
  (cond
    [else (if (or (>= (posn-y (ufo-position (world-ufo w)))
                      (- SCENE-SIZE (* RADIUS 2)))
                  (ufo-missile-collision? w))
              #true
              #false)]))

; World -> Image
; consumes a world w and outputs the final scene

(check-expect (last-picture (make-world (make-tank (make-posn 10 10) '())
                                        (make-ufo (make-posn 100 100) "")))
              (place-image
               LAST-MSG (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
               (render-world (make-world (make-tank (make-posn 10 10) '())
                                         (make-ufo (make-posn 100 100) "")))))

(define (fn-last-picture w)
  (... ... ... ... (render-world (make-world (world-tank w)
                                             (world-ufo w)))))

(define (last-picture w)
  (place-image LAST-MSG (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
               (render-world (make-world (world-tank w)
                                         (world-ufo w)))))

; World -> Boolean
; consumes a world w and outputs true if a a missile and ufo
; collide

(check-expect (ufo-missile-collision?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 200 100) "")))
              #true)

(check-expect (ufo-missile-collision?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 200 190) "")))
              #false)

(check-expect (ufo-missile-collision?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 180 100) "")))
              #true)

(define (fn-ufo-missile-collision? w)
  (cond
    [(empty? (tank-missile (world-tank w))) ...]
    [else (if (... w (... (tank-missile (world-tank w))))
              ...
              (fn-ufo-missile-collision?
               (make-world
                (make-tank (tank-position (world-tank w))
                           (rest (tank-missile (world-tank w))))
                (world-ufo w))))]))

(define (ufo-missile-collision? w)
  (cond
    [(empty? (tank-missile (world-tank w))) #false]
    [else (if (encounter-hitbox? w (first (tank-missile (world-tank w))))
              #true
              (ufo-missile-collision?
               (make-world
                (make-tank (tank-position (world-tank w))
                           (rest (tank-missile (world-tank w))))
                (world-ufo w))))]))

; World Posn -> Boolean
; consumes a world w and a Posn p and outputs true if the hitboxes
; overlap

(check-expect (encounter-hitbox?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 220 100) ""))
               (make-posn 200 100))
              #true)

(check-expect (encounter-hitbox?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 180 100) ""))
               (make-posn 200 100))
              #true)

(check-expect (encounter-hitbox?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 80) '()))
                           (make-ufo (make-posn 200 100) ""))
               (make-posn 200 80))
              #true)

(check-expect (encounter-hitbox?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 110) '()))
                           (make-ufo (make-posn 200 100) ""))
               (make-posn 200 110))
              #true)

(check-expect (encounter-hitbox?
               (make-world (make-tank (make-posn 0 0)
                                      (cons (make-posn 200 100) '()))
                           (make-ufo (make-posn 160 160) ""))
               (make-posn 200 100))
              #false)

(define (fn-encounter-hitbox? w p)
  (cond
    [(and (... (... (posn-y (ufo-position (world-ufo w))) ...)
               (posn-y p))
          (... (posn-y (ufo-position (world-ufo w)))
               (... (posn-y p) ...))
          (... (... (posn-x (ufo-position (world-ufo w))) ...)
               (posn-x p))
          (... (posn-x (ufo-position (world-ufo w)))
               (... (posn-x p) ...)))
     ...]
    [else
     ...]))

(define (encounter-hitbox? w p)
  (cond
    [(and (>= (+ (posn-y (ufo-position (world-ufo w))) (+ RADIUS (/ RADIUS 4)))
              (posn-y p))
          (<= (posn-y (ufo-position (world-ufo w)))
              (+ (posn-y p) (+ RADIUS (/ RADIUS 4))))
          (>= (+ (posn-x (ufo-position (world-ufo w))) (+ RADIUS (/ RADIUS 4)))
              (posn-x p))
          (<= (posn-x (ufo-position (world-ufo w)))
              (+ (posn-x p) (+ RADIUS (/ RADIUS 4)))))
     #true]
    [else
     #false]))

; World -> World
; consumes a world w and outputs new world with an updated direction for ufo

(define (fn-direction-random w)
  (cond
    [else (if (zero? (random ...))
              (make-world (world-tank w)
                          (make-ufo (ufo-position (world-ufo w)) ...))
              (make-world (world-tank w)
                          (make-ufo (ufo-position (world-ufo w)) ...)))]))

(define (direction-random w)
  (cond
    [else (if (zero? (random 2))
              (make-world (world-tank w)
                          (make-ufo
                           (ufo-position (world-ufo w)) "left"))
              (make-world (world-tank w)
                          (make-ufo
                           (ufo-position (world-ufo w)) "right")))]))

; World -> Image
; consumes a world w and outputs a start-screen

(check-expect (start (make-world (make-tank (make-posn 0 0) '())
                                 (make-ufo (make-posn 0 0) "")))
              (place-image START-MSG
                           (/ SCENE-SIZE 2) (/ SCENE-SIZE 3)
                           (place-image
                            INSTRUCTION-MSG1
                            (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
                            (place-image INSTRUCTION-MSG2
                                         (/ SCENE-SIZE 2)
                                         (/ SCENE-SIZE 1.5)
                                         (place-image INSTRUCTION-MSG3
                                                      (/ SCENE-SIZE 2)
                                                      (/ SCENE-SIZE 1.25)
                                                      SCENE)))))
(define (fn-start w)
  (... START-SCREEN
       (/ SCENE-SIZE 2) (/ SCENE-SIZE 3)
       (...
        INSTRUCTION-MSG1
        (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
        (... INSTRUCTION-MSG2
             (/ SCENE-SIZE 2) (/ SCENE-SIZE 1.5)
             SCENE))))

(define (start w)
  (place-image START-MSG
               (/ SCENE-SIZE 2) (/ SCENE-SIZE 3)
               (place-image
                INSTRUCTION-MSG1
                (/ SCENE-SIZE 2) (/ SCENE-SIZE 2)
                (place-image INSTRUCTION-MSG2
                             (/ SCENE-SIZE 2) (/ SCENE-SIZE 1.5)
                             (place-image INSTRUCTION-MSG3
                                          (/ SCENE-SIZE 2) (/ SCENE-SIZE 1.25)
                                          SCENE)))))

; World -> Boolean
; consumes a world w and outputs true if the game is in its initial state
; for the first time

(check-expect (start?
               (make-world (make-tank
                            (make-posn
                             (/ SCENE-SIZE 2)
                             (- SCENE-SIZE (+ SIZE (/ SIZE 2))))
                            '())
                           (make-ufo (make-posn (/ SCENE-SIZE 2) SIZE) "")))
              #true)

(check-expect (start?
               (make-world (make-tank
                            (make-posn
                             (/ SCENE-SIZE 2)
                             (- SCENE-SIZE (+ SIZE (/ SIZE 2))))
                            '())
                           (make-ufo (make-posn (/ SCENE-SIZE 2) SIZE) "left")))
              #false)

(define (fn-start? w)
  (cond
    [else (if (equal? WORLD2 w)
              ... ...)]))
              

(define (start? w)
  (cond
    [else (if (equal? WORLD2 w)
              #true #false)]))

; World -> Boolean
; consumes a world w and outputs true if the the ufo
; moves beyond the screen boudnary

(check-expect (ufo-exceed-x? (make-world (make-tank (make-posn 0 0) '())
                                         (make-ufo (make-posn 60 60)
                                                   "")))
              #false)

(check-expect (ufo-exceed-x? (make-world (make-tank (make-posn 0 0) '())
                                         (make-ufo (make-posn 20 20)
                                                   "")))
              #false)

(check-expect (ufo-exceed-x? (make-world (make-tank (make-posn 0 0) '())
                                         (make-ufo (make-posn 380 60)
                                                   "")))
              #false)

(check-expect (ufo-exceed-x? (make-world (make-tank (make-posn 0 0) '())
                                         (make-ufo (make-posn 10 100)
                                                   "")))
              #true)

(check-expect (ufo-exceed-x? (make-world (make-tank (make-posn 0 0) '())
                                         (make-ufo (make-posn 390 60)
                                                   "")))
              #true)

(define (fn-ufo-exceed-x? w)
  (cond
    [(< (- (posn-x (ufo-position (world-ufo w))) ...) ...)
     ...]
    [(> (+ (posn-x (ufo-position (world-ufo w))) ...) ...)
     ...]
    [else
     ...]))

(define (ufo-exceed-x? w)
  (cond
    [(< (- (posn-x (ufo-position (world-ufo w))) RADIUS) 0)
     #true]
    [(> (+ (posn-x (ufo-position (world-ufo w))) RADIUS) SCENE-SIZE)
     #true]
    [else
     #false]))

; World -> World
; consumes a world w and generates a new world with a random x positon
; for the ufo not exceeding the screen boundary

(check-random (ufo-random-x (make-world (make-tank (make-posn 0 0) '())
                                        (make-ufo (make-posn 10 20)
                                                  "")))
              (make-world (make-tank (make-posn 0 0) '())
                          (make-ufo (make-posn
                                     (check-x (+ SCENE-SIZE 1))
                                     20)
                                    "")))

(define (fn-ufo-random-x w)
  (make-world (world-tank w)
              (make-ufo
               (make-posn
                (check-x (random (... ... ...)))
                (posn-y (ufo-position (world-ufo w)))) ...)))

(define (ufo-random-x w)
  (make-world (world-tank w)
              (make-ufo
               (make-posn
                (check-x (random (+ SCENE-SIZE 1)))
                (posn-y (ufo-position (world-ufo w)))) "")))

; Number -> Number
; comsumes a number n and checks if it is within the screen boundary and
; and a multiple of 20

(define (check-x n)
  (cond
    [(and (zero? (modulo n SIZE))
          (not (ufo-exceed-x?
                (make-world (make-tank (make-posn 0 0) '())
                            (make-ufo (make-posn n 0) "")))))
     n]
    [else (check-x (random (+ SCENE-SIZE 1)))]))

; main function

(define (ender-main rate)
  (big-bang WORLD2
    [on-tick tock rate]
    [to-draw render-world]
    [on-key control]
    [stop-when last-world? last-picture]
    [state #t]
    [name "Command"]
    [close-on-stop 3]))

; usage
; (ender-main 1)
