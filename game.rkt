#lang racket
(require racket/gui)
(provide game%)
(require "hash-tables.rkt")


;; PURPOSE: game% is a class which handles the game's
;;          physics, logic and, indirectly by method calls, drawing.
;;
;; LAST MODIFIED: 16-05-16
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand


(define game%
  (class object%

    ;; METHOD: screen-wrap
    ;;
    ;; DESCIPTION: A method used to simulate
    ;;             that the screen "wraps around the edges",
    ;;             i.e. when an object, obj, passes outside
    ;;             of the screen on one side, it reappears
    ;;             at the opposite side when the middle of the object
    ;;             passes through the edge.
    ;;
    ;; INPUT: obj - any object with the methods get-mid-xpos and set-mid-x!.
    ;;
    ;; OUTPUT: #<void>
    (define (screen-wrap obj)
      (let* ([xpos (send obj get-mid-xpos)]
             [ypos (send obj get-mid-ypos)])
        (cond
          [(> xpos 1920) (send obj set-mid-x! 0)]
          [(< xpos 0) (send obj set-mid-x! 1920)]
          [(> ypos 1080) (send obj set-mid-y! 0)]
          [(< ypos 0) (send obj set-mid-y! 1080)])))
    

    ;; METHOD: key-handler
    ;;
    ;; DESCRIPTION: When the key key-code is pressed/released it's corresponding
    ;;              value in key-hash is set to #t/#f. This means we can
    ;;              determine if several keys are pressed and handle every legal
    ;;              key combination. See *****.
    ;;
    ;; INPUT: key-code - a key code symbol.
    ;;        val - a boolean value, i.e. #t or #f.
    ;;
    ;; OUTPUT: #<void>
    (define/public (key-handler key-code val)
      (hash-set! key-hash key-code val))
    

    ;; METHOD: collision?
    ;;
    ;; DESCRIPTION: A method that checks if the distance between the centers of
    ;;              two objects is smaller than the sum of the objects' radii.
    ;;              If so the method returns #t, signaling that the objects have
    ;;              collided.
    ;;
    ;; INPUT: obj-1 - any object with the field radius and the methods
    ;;                get-mid-xpos and get-mid-ypos.
    ;;        obj-2 - any object with the field radius and the methods
    ;;                get-mid-xpos and get-mid-ypos.
    ;;
    ;; OUTPUT: a boolean value, i.e. #t or #f.
    (define (collision? obj-1 obj-2)
      (let ([radius-1 (get-field radius obj-1)]
            [radius-2 (get-field radius obj-2)]
            [mid-xpos-1 (send obj-1 get-mid-xpos)]
            [mid-xpos-2 (send obj-2 get-mid-xpos)]
            [mid-ypos-1 (send obj-1 get-mid-ypos)]
            [mid-ypos-2 (send obj-2 get-mid-ypos)])
        
        (<= (sqrt (+ (sqr (- mid-xpos-1 mid-xpos-2))
                     (sqr (- mid-ypos-1 mid-ypos-2))))
            (+ radius-1 radius-2))))

    
    ;; METHOD: draw-stats
    ;;
    ;; DESCRIPTION: A method responsible for drawing the values
    ;;              of some of the ship objects' fields on a
    ;;              drawing context, providing the player
    ;;              with information about the state of the game
    ;;
    ;; INPUT: dc - a drawing context.
    ;;
    ;; OUTPUT: #<void>
    (define (draw-stats dc)
      (send dc set-text-foreground "white")
      (define xpos 10)
      (for-each (lambda (ship)
                  (let ([score (number->string (send ship get-score))]
                        [lives (number->string (send ship get-lives))]
                        [level (number->string level)])
                    (send dc draw-text (string-append "Level: " level) xpos 10)
                    (send dc draw-text (string-append "Score: " score) xpos 30)
                    (send dc draw-text (string-append "Lives: " lives) xpos 50)
                    (set! xpos (+ xpos 200))))
                (hash-values ship-hash)))

    
    ;; METHOD: end-of-level
    ;;
    ;; DESCRIPTION: A method which determines wether the game is over
    ;;              or if the current level has been completed by checking
    ;;              if ship-hash or asteroids-hash is empty.
    ;;
    ;; INPUT: dc - a drawing context.
    ;;
    ;; OUTPUT: #<void>
    (define (end-of-level dc)
      (send dc set-text-foreground "white")
      (when (hash-empty? ship-hash)
        (send dc draw-text "Game over!" 920 540)
        (send (new timer% [notify-callback game-over!]) start 5000))
      (when (hash-empty? asteroids-hash)
        (level-completed! #t)))
    
    
    ;; METHOD: render
    ;;
    ;; DESCRIPTION: A rendering method. Is called by the on-paint method
    ;;              provided by the game-canvas% class. For every object, except
    ;;              game objects, the method calls the update method, to draw
    ;;              the objects and calculate their new positions, and the
    ;;              collision? method to check for collisons. On collision
    ;;              between two objects the  obj-has-collided-with method is
    ;;              called. It will also call the draw-stats method to update
    ;;              the "HUD" once every cycle.
    ;;               
    ;; INPUT: dc - a drawing context
    ;;
    ;; OUTPUT: #<void>
    (define/public (render dc)
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
    (super-new)))





