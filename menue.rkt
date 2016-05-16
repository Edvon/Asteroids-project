#lang racket
(require racket/gui)
(provide *menue-window*)


;; Creates an instance of the frame% class, *menue-window*,
;; on wich we can put panels to put buttons and canvases in.

(define *menue-window*
  (new frame%
       [label "Main Menue"]
       [width 300]
       [height 400]))

;; Creates an instance of the panel% class on wich we can put a canvas
(define *menue-image-panel*
  (new panel%
       [parent *menue-window*]
       [alignment '(center top)]
       [min-height 200]))

;; Generates a canvas on wich a bitmap can be printed
(define *menue-image-canvas*
  (new canvas%
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
                   (send button set-label "new label")
                   (send *menue-window* show #f))]))

(define 2-players-button
  (new button%
       [parent *buttons-panel*]
       [label "2 players"]
       [callback (lambda (button event)
                   (send button set-label "new label")
                   (send *menue-window* show #f))]))

(define reset-button
  (new button%
       [parent *buttons-panel*]
       [label "reset"]
       [callback (lambda (button event)
                   (send button set-label "new label")
                   (send *menue-window* show #f))]))

(define quit-button
  (new button%
       [parent *buttons-panel*]
       [label "quit"]
       [callback (lambda (button event)
                   (send button set-label "new label")
                   (send *menue-window* show #f))]))

