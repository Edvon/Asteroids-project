#lang racket
(require racket/gui)
(require "hash-tables.rkt")
(require "ship.rkt")
(require "asteroids.rkt")
(require "game.rkt")


;; Create an instance of the ship%-class which is to be
;; controlled by the player.
(define *player-1*
  (make-object ship%))

(define *player-2*
  (make-object ship% 9 (list #\i #\j #\l #\m)))


;; Create an instance of the game%-class which is supposed
;; to handle all physics, logic and rendering.
(define *game-physics*
  (make-object game%))

;;; Initialize the game. Currently all this entails is
;; creating some asteroids. Procedure to be expanded and revised.
(define (init-game)
  (make-object asteroid%)
  (make-object asteroid%))

;; Create an instance of the frame% class, *game-window*, on which we can put a
;; canvas.
(define *asteroids-window* (new frame%
                                [label "Asteroids"]
                                [width 1920]
                                [height 1080]))


;; Define a class, game-canvas%, by inheriting from canvas%. game-canvas%
;; is defined to call a some method, keyboard-handler, when a key-event occurs,
;; i.e. when a key is pressed.
(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler display])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

;; Define an instance of the game-canvas% class. We define the behavior
;; of the keyboard-handler to call the move method in the ship% class.
;; This might prove be inconvenient were we to implement a menu, since
;; then not every key-event would be related to movement. We define the
;; paint-callback argument of the on-paint method to be a call to the render
;; method in the game% class. We provide the render method with all our
;; created class objects so they can be rendered when we update the canvas.
(define *asteroids-game-canvas*
  (new game-canvas%
       [parent *asteroids-window*]
       [paint-callback (lambda (canvas dc)
                         (send *game-physics* render dc))]
       
       [keyboard-handler (lambda (key-event)
                           (let ([key-code (send key-event get-key-code)]
                                 [key-release-code (send key-event get-key-release-code)])
                             
                             (if (equal? key-code 'release)
                                 (send *game-physics* key-handler key-release-code #f)
                                 (send *game-physics* key-handler key-code #t))))]))

;; Define a timer which on every callback refreshes the canvas.
(define *game-timer* (new timer%
                          [notify-callback (lambda ()
                                             (send *asteroids-game-canvas*
                                                   refresh))]))

;; Define a procedure for initializing and starting the game. It does this by
;; showing the window and then starting the timer so we can refresh the canvas.
(define (start-game)
  (init-game)
  (send *asteroids-game-canvas* set-canvas-background (send the-color-database find-color "black"))
  (send *asteroids-window* show #t)
  (send *game-timer* start 50)
  (send *asteroids-game-canvas* focus))