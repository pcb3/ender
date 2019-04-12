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
(define CAPSULE (rectangle (/ RADIUS 4) RADIUS 'solid 'gray))
(define SURFACE (rectangle (- SCENE-SIZE 1) (- HEIGHT 1) 'solid 'lightgray))
(define SCENE
  (place-image SURFACE (/ SCENE-SIZE 2) (- SCENE-SIZE (/ HEIGHT 2)) MT))
