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
(define LAST-MSG (text "GAME OVER" SIZE 'black))

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE 'darkgray))
(define UFO (circle RADIUS 'solid 'gray))
(define TANK (square SIZE 'solid 'white))
(define MISSILE
  (overlay
   (rectangle (- (/ RADIUS 4) 2) (- (/ RADIUS 2) 2) 'solid 'white)
   (rectangle (/ RADIUS 4) (/ RADIUS 2) 'solid 'gray)))
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
(define WORLD2 (make-world (make-tank (make-posn (/ SCENE-SIZE 2)
                                                 (- SCENE-SIZE (+ RADIUS (/ RADIUS 2))))
                                      '())
                           (make-ufo (make-posn (/ SCENE-SIZE 2) RADIUS) "")))

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
    [(string=? key "right")
     (make-world
      (make-tank (make-posn (+ (posn-x (tank-position (world-tank w))) SIZE)
                            (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key "left")
     (make-world
      (make-tank (make-posn (- (posn-x (tank-position (world-tank w))) SIZE)
                            (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key " ")
     (make-world
      (make-tank (tank-position (world-tank w))
                 (cons (make-posn (posn-x (tank-position (world-tank w)))
                                  (- SCENE-SIZE (+ (* RADIUS 2) (/ RADIUS 2))))
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

(check-expect (tock (make-world (make-tank (make-posn 0 0) '())
                                (make-ufo (make-posn 10 10) "")))
              (make-world
               (make-tank (make-posn 0 0) '())
               (make-ufo (make-posn 10 (+ 10 SIZE)) "")))

(check-expect (tock (make-world (make-tank (make-posn 0 0)
                                           (cons (make-posn 20 40) '()))
                                (make-ufo (make-posn 10 10) "")))
              (make-world
               (make-tank (make-posn 0 0)
                          (cons (make-posn 20 (- 40 SIZE)) '()))
               (make-ufo (make-posn 10 (+ 10 SIZE)) "")))

(check-expect (tock (make-world (make-tank (make-posn 0 0)
                                           (cons (make-posn 20 40) '()))
                                (make-ufo (make-posn 10 10) "right")))
              (make-world
               (make-tank (make-posn 0 0)
                          (cons (make-posn 20 (- 40 SIZE)) '()))
               (make-ufo
                (make-posn (+ 10 SIZE) (+ 10 SIZE)) "right")))

(check-expect (tock (make-world (make-tank (make-posn 0 0)
                                           (cons (make-posn 20 40) '()))
                                (make-ufo (make-posn 40 40) "left")))
              (make-world
               (make-tank (make-posn 0 0)
                          (cons (make-posn 20 (- 40 SIZE)) '()))
               (make-ufo
                (make-posn (- 40 SIZE) (+ 40 SIZE)) "left")))

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
  (make-world
   (make-tank (tank-position (world-tank w))
              (cond
                [else (if (empty? (tank-missile (world-tank w)))
                          (tank-missile (world-tank w))
                          (update-missile (tank-missile (world-tank w))))]))
   (make-ufo
    (make-posn
     (cond
       [(string=? "right" (ufo-direction (world-ufo w)))
            (+ (posn-x (ufo-position (world-ufo w))) SIZE)]
       [(string=? "left" (ufo-direction (world-ufo w)))
            (- (posn-x (ufo-position (world-ufo w))) SIZE)]
       [else (posn-x (ufo-position (world-ufo w)))])
     (+ (posn-y (ufo-position (world-ufo w))) SIZE))
    (ufo-direction (world-ufo w)))))

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
                           (make-ufo (make-posn 10 (- SCENE-SIZE (- RADIUS 1))) "")))
              #true)

(define (fn-last-world? w)
  (cond
    [else (if (>= (posn-y (ufo-position (world-ufo w))) (- SCENE-SIZE (* RADIUS 2)))
              ...
              ...)]))

(define (last-world? w)
  (cond
    [else (if (>= (posn-y (ufo-position (world-ufo w))) (- SCENE-SIZE (* RADIUS 2)))
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
