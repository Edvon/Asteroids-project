#lang racket
(require racket/gui)
(require "main.rkt")

;; Define a procedure for initializing and starting the game. It does this by
;; showing the window and then starting the timer so we can refresh the canvas.
(define (start-game)
  (init-level level)
  (send *asteroids-game-canvas* set-canvas-background (send the-color-database find-color "black"))
  (send *asteroids-window* show #t)
  (send *game-timer* start 50)
  (send *asteroids-game-canvas* focus))

;; Define a procedure wich sends up the start-menue
(define (start-menue)
  (send *menue-window* show #t)
  (send *game-timer* stop))