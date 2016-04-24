#lang racket
(require racket/gui)
(require "ship.rkt")
(require "game.rkt")

(define *game-window* (new frame%
                         [label "Asteroids"]
                         [width 1920]
                         [height 1080]))
(define *player-1*
  (new ship%))

(define *player-1-physics*
  (new game%
       [obj *player-1*]))

(define *player-2*
  (new ship%
        [xpos 0]
        [ypos 0]))

(define *player-2-physics*
  (new game%
       [obj *player-2*]))



(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler display])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

(define *my-game-canvas*
  (new game-canvas%
       [parent *game-window*]
       [paint-callback (lambda (canvas dc)
                         (send *player-1-physics* render dc)
                         (send *player-2-physics* render dc))]
       [keyboard-handler (lambda (key-event)
                           (let ((key-code (send key-event get-key-code)))
                             (if (not (equal? key-code 'release))
                                 (send *player-1* key-handler key-event)
                                 (void))))]))

(define *game-timer* (new timer%
                        [notify-callback (lambda ()
                                           (send *my-game-canvas* refresh))]))

(send *game-window* show #t)
(send *game-timer* start 17)
(send *my-game-canvas* focus)