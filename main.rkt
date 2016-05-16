#lang racket
(require racket/gui)
(require "hash-tables.rkt")
(require "ship.rkt")
(require "ufo.rkt")
(require "asteroids.rkt")
(require "game.rkt")


;;------------------------Menue---------------------
;; Creates an instance of the frame% class, *menue-window*,
;; on wich we can put panels to put buttons and canvases in.

(define *menue-window*
  (new frame%
       [label "Main Menue"]
       [width 300]
       [height 375]))

;; Creates an instance of the panel% class on wich we can put a canvas
(define *menue-image-panel*
  (new panel%
       [parent *menue-window*]
       [alignment '(center top)]
       [min-height 150]))

;; Creates a bitmap from our desired .png file.
(define *menue-bitmap*
  (make-object bitmap%
    "asteroids-menue.png"))

;; Generates a canvas on wich a bitmap can be printed
(define *menue-image-canvas* 
  (new canvas%
       [paint-callback (lambda (canvas dc)
                         (send dc draw-bitmap *menue-bitmap* 0 0))]
       [parent *menue-image-panel*]))

;; A panel on wich menue-buttons will be placed
(define *buttons-panel*
  (new vertical-panel%
       [parent *menue-window*]
       [alignment '(center bottom)]))


;; Instances of the button% class is created to be able to control the gameplay.

(define 1-player-button
  (new button%
       [parent *buttons-panel*]
       [label "1 player"]
       [callback (lambda (button event)
                   (start-game)
                   (send *menue-window* show #f))]))

(define 2-players-button
  (new button%
       [parent *buttons-panel*]
       [label "2 players"]
       [callback (lambda (button event)
                   (define *player-2*
                     (make-object ship% 9 '(#\i #\j #\l #\return #\backspace )))
                   (start-game)
                   (send *menue-window* show #f))]))

(define quit-button
  (new button%
       [parent *buttons-panel*]
       [label "quit"]
       [callback (lambda (button event)
                   (exit))]))

;;------------------------Game----------------------

;; Define a timer which will control when
;; ufos appear.
(define *ufo-appear-timer*
  (new timer%
       [notify-callback (lambda ()
                          (when (hash-empty? ufo-hash)
                            (make-object ufo%)))]))

;; Create an instance of the ship%-class which is to be
;; controlled by the player.
(define *player-1*
  (make-object ship%))

;; Create an instance of the game%-class which is supposed
;; to handle all physics, logic and rendering.
(define *game-physics*
  (make-object game%))

;;; Initialize the game. Currently all this entails is
;; creating some asteroids and starting the ufo-appear-timer.
;; Procedure to be expanded and revised.

(define (init-level level)
  (level-completed! #f)
  (send *ufo-appear-timer* start 30000)
  (for ([i (+ level 2)])
    (make-object asteroid%)))

(define (on-level-over)
  (cond
    [game-over? (exit)]
    [level-completed? (update-level!)
                      (init-level level)]))

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
                         (send *game-physics* render dc)
                         (on-level-over))]
       
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
