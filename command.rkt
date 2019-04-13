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
(define MT (empty-scene SCENE-SIZE SCENE-SIZE 'black))
(define SHIP (circle RADIUS 'solid 'gray))
(define CAPSULE
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
