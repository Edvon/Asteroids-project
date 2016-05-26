#lang racket
(require racket/gui)
(require "utilities.rkt")
(require "ufo.rkt")
(require "ship.rkt")
(require "asteroids.rkt")


;; PURPOSE: main.rkt is where windows, canvases and frames are defined. It also
;;          handles game initialization and refreshing the canvas periodically by
;;          calling the render procedure.
;;
;; LAST MODIFIED: 16-05-25
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.


;;------------------------------- Menu graphics --------------------------------

;; Creates an instance of the frame% class, *menu-window*,
;; on which we can put panels to put buttons and canvases in.
(define *menu-window*
  (new frame%
       [label "Main menu"]
       [width 300]
       [height 375]))

;; Defines an instance of the panel% class on wich we can put a canvas.
(define *menu-image-panel*
  (new panel%
       [parent *menu-window*]
       [alignment '(center top)]
       [min-height 150]))

;; Defines a bitmap from our desired .png file.
(define *menu-bitmap*
  (make-object bitmap%
    "asteroids-menu.png"))

;; Generates a canvas on wich a bitmap can be printed.
(define *menu-image-canvas* 
  (new canvas%
       [paint-callback (lambda (canvas dc)
                         (send dc draw-bitmap *menu-bitmap* 0 0))]
       [parent *menu-image-panel*]))

;; A panel on wich menu-buttons will be placed
(define *buttons-panel*
  (new vertical-panel%
       [parent *menu-window*]
       [alignment '(center bottom)]))


;; A button which will start the game in single-player mode.
(define 1-player-button
  (new button%
       [parent *buttons-panel*]
       [label "1 player"]
       [callback (lambda (button event)
                   (define *player-1*
                     (make-object ship%))
                   (start)
                   (send *menu-window* show #f))]))

;; A button which will start the game in two-player mode. When we create the
;; second player we define which keys the player will use to steer it's ship
;; with.
(define 2-players-button
  (new button%
       [parent *buttons-panel*]
       [label "2 players"]
       [callback (lambda (button event)
                   (define *player-2*
                     (make-object ship%
                       "Player 2" '(#\i #\j #\l #\return #\backspace )))
                   (define *player-1*
                     (make-object ship%))
                   (start)
                   (send *menu-window* show #f))]))

;; A button which will start the game in two-player mode.
(define quit-button
  (new button%
       [parent *buttons-panel*]
       [label "quit"]
       [callback (lambda (button event)
                   (exit))]))


;;------------------------------ Game graphics ---------------------------------

;; Create an instance of frame%, *asteroids-window*, on which we can put a
;; canvas.
(define *asteroids-window* (new frame%
                                [label "Asteroids"]
                                [width 1920]
                                [height 1080]))


;; Define a class, game-canvas%, by inheriting from canvas%. game-canvas%
;; is supposed to call some procedure when a key event occurs,
;; i.e. when a key is pressed, providing the key-event as an argument to the
;; procedure.

(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler display])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

;; We create an instance of game-canvas%, *asteroids-game-canvas*, specifying
;; the lambda procedure, which is to be called on a key-event, to get the key
;; code of the event and provide it as an argument to the key-handler procedure
;; in utilities.rkt. The paint-callback lambda procedure calls render, with the
;; canvas' drawing context as an argument, and on-level-over, in utilities.rkt,
;; to check if the level is completed or if the game is over.
(define *asteroids-game-canvas*
  (new game-canvas%
       [parent *asteroids-window*]
       [paint-callback (lambda (canvas dc)
                         (render dc)
                         (on-level-over))]       
       [keyboard-handler
        (lambda (key-event)
          (let ([key-code (send key-event get-key-code)]
                [key-release-code (send key-event get-key-release-code)])
            (if (equal? key-code 'release)
                (key-handler key-release-code #f)
                (key-handler key-code #t))))]))


;;------------------------------ Rendering and logic ---------------------------

;; on-level exits the game if game-over?, in utilities.rkt, is true. If
;; level-completed?, also in utilities.rkt, is true it updates the variable
;; level in utilities.rkt and initializes a new level with init-level.
(define (on-level-over)
  (cond
    [game-over? (exit)]
    [level-completed? (update-level!)
                      (init-level level)]))

;; PROCEDURE: render
;;
;; DESCRIPTION: A rendering procedure. Is called by the on-paint method provided
;;              by the game-canvas% class. For every object the procedure call
;;              the update method, to draw the objects and calculate their new
;;              positions, and the collision? procedure to check for collisons.
;;              On collision between two objects the obj-has-collided-with
;;              method is called for the collided objects. It will also call the
;;              procedure draw-stats to update the "HUD" once every cycle.
;;               
;; INPUT: dc - a drawing context
;;
;; OUTPUT: #<void>
(define (render dc)
  (let* ([all-obj-lst (append (hash-values ship-hash)
                              (hash-values bullets-hash)
                              (hash-values asteroids-hash)
                              (hash-values ufo-hash))])
    (define (update-obj-lst)
      (set! all-obj-lst (append (hash-values ship-hash)
                                (hash-values bullets-hash)
                                (hash-values asteroids-hash))))
    (for-each (lambda (obj)
                (send obj update! dc)
                (screen-wrap obj))
              all-obj-lst)
    
    (for-each (lambda (obj1)
                (for-each (lambda (obj2)
                            (unless (equal? (get-field id obj1)
                                            (get-field id obj2))
                              (when (collision? obj1 obj2)
                                (send obj1 obj-has-collided-with obj2)
                                (send obj2 obj-has-collided-with obj1)
                                (update-obj-lst))))
                          all-obj-lst))
              all-obj-lst)
    (draw-stats dc)
    (end-of-level dc)))

;; --------------------------------- Timers ------------------------------------

;; Define a timer which will control when ufos appear.
(define *ufo-appear-timer*
  (new timer%
       [notify-callback (lambda ()
                          (when (hash-empty? ufo-hash)
                            (make-object ufo%)))]))

;; Define a timer which on every callback refreshes the canvas.
(define *game-timer* (new timer%
                          [notify-callback (lambda ()
                                             (send *asteroids-game-canvas*
                                                   refresh))]))

;;--------------------- Game initilization and game start ----------------------

;; PROCEDURE: init-level
;;
;; DESCRIPTION: Initializes a level, specified by level, by starting
;;              *ufo-appear-timer* and creating a certain number of asteroid
;;              objects.
;;               
;; INPUT: level - an integer.
;;
;; OUTPUT: (object:asteroid% ...)
(define (init-level level)
  (level-completed! #f)
  (send *ufo-appear-timer* start 30000)
  (for ([i (+ level 2)])
    (make-object asteroid%)))

;; PROCEDURE: start
;;
;; DESCRIPTION: We define a procedure for starting the game. On game start we
;;              call the init-level procedure to initilize the first level,
;;              show the game window and then start a timer so we can refresh
;;              the canvas periodically.
;;
;; INPUT: None.
;;
;; OUTPUT: #<void>
(define (start)
  (init-level level)
  (send *asteroids-game-canvas* set-canvas-background
        (send the-color-database find-color "black"))
  (send *asteroids-window* show #t)
  (send *game-timer* start 50)
  (send *asteroids-game-canvas* focus))

;; Define a procedure which shows the game menu.
(define (start-game)
  (send *menu-window* show #t))