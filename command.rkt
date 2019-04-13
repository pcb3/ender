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
(define HEIGHT 20)
(define SCENE-SIZE (* WIDTH HEIGHT))

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE 'darkgray))
(define UFO (circle RADIUS 'solid 'gray))
(define TANK (square WIDTH 'solid 'white))
(define MISSILE
  (overlay
   (rectangle (- (/ RADIUS 4) 2) (- RADIUS 2) 'solid 'white)
   (rectangle (/ RADIUS 4) RADIUS 'solid 'gray)))
(define SURFACE (rectangle (- SCENE-SIZE 1) (- HEIGHT 1) 'solid 'lightgray))
(define SCENE
  (place-image SURFACE (/ SCENE-SIZE 2) (- SCENE-SIZE (/ HEIGHT 2)) MT))

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
    [else (place-image (posn-x (first (tank-missile (world-tank w))))
               (posn-y  (first (tank-missile (world-tank w))))
               (fn-render-world
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
                                     (cons (make-posn 0 0) '()))
                                   (make-ufo (make-posn 1 1) "")))

(check-expect (control (make-world (make-tank (make-posn 0 0) '())
                                   (make-ufo (make-posn 1 1) "")) "right")
              (make-world (make-tank (make-posn 1 0) '())
                                   (make-ufo (make-posn 1 1) "")))

(check-expect (control (make-world (make-tank (make-posn 3 0) '())
                                   (make-ufo (make-posn 1 1) "")) "left")
              (make-world (make-tank (make-posn 2 0) '())
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
      (make-tank (make-posn (add1 (posn-x (tank-position (world-tank w))))
                 (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key "left")
     (make-world
      (make-tank (make-posn (sub1 (posn-x (tank-position (world-tank w))))
                 (posn-y (tank-position (world-tank w))))
                 (tank-missile (world-tank w)))
      (world-ufo w))]
    [(string=? key " ")
     (make-world
      (make-tank (tank-position (world-tank w))
                 (cons (make-posn 0 0) (tank-missile (world-tank w))))
      (world-ufo w))]
    [else w]))

(define (ender-main rate)
  (big-bang WORLD0
    ;[on-tick tock rate]
    [to-draw render-world]
    [on-key control]
    ;[stop-when last-world-connected? last-picture]
    [state #t]
    [name "Ender"]))

; usage
; (ender-main 1)
