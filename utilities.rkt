#lang racket
(require racket/gui)
(provide (all-defined-out))

;; PURPOSE: Utilities is where we store useful procedures and hash-tables
;; so they can be called and used in all modules.
;;
;; LAST MODIFIED: 16-05-19
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.


;;------------------------------- Hash tables ----------------------------------
;; We use hash tables to store stuff throughout the project
;; because they're mutable, and it turns out to be rather convenient.

;; A hash table containing key codes and corresponding booleans. Used to store
;; information about key presses.
(define key-hash (make-hash))

;; A hash table containing every "living" ship object and their names.
(define ship-hash (make-hash))

;; A hash table containing every "dead" ship object and their names.
(define dead-ship-hash (make-hash))

;; A hash table contaning every asteroid object and their names.
(define asteroids-hash (make-hash))

;; A hash table contaning every bullet object and their names.
(define bullets-hash (make-hash))

;; A hash table contaning every ufo object and their names.
(define ufo-hash (make-hash))



;; ------------------------------- Level handling ------------------------------

;; Initialize level-completed? and define it to be false.
(define level-completed? #f)

;; A set:er procedure used to change the value of level-completed? to val.
(define (level-completed! val)
  (set! level-completed? val))

;; Initialize game-over? and define it to be false.
(define game-over? #f)

;; A set:er procedure used to change the value of game-over? to true.
(define (game-over!)
  (set! game-over? #t))

;; PROCEDURE: end-of-level
;;
;; DESCRIPTION: A procedure which determines wether the game is over
;;              or if the current level has been completed by checking
;;              if ship-hash or asteroids-hash is empty. Also draws stats
;;              when the game is over.
;;
;; INPUT: dc - a drawing context.
;;
;; OUTPUT: #<void>
(define (end-of-level dc)
  (define ypos 600)
  (send dc set-text-foreground "white")
  (when
      (or (hash-empty? ship-hash)
          (hash-ref key-hash 'escape))
    (send dc draw-text "Game over!" 900 540)
    (send dc draw-text (string-append "Level " (number->string level))
          915 560)
    (for-each (lambda (ship)                
                (let ([name (get-field name ship)]
                      [score (number->string (send ship get-score))])
                  (send dc draw-text name 870 ypos)
                  (send dc draw-text (string-append "Score: " score) 940 ypos)
                  (set! ypos (+ ypos 20))))
              (hash-values dead-ship-hash))
    (send (new timer% [notify-callback game-over!]) start 8000))
  (when (hash-empty? asteroids-hash)
    (level-completed! #t)))

;; Upon game initialization the level variable is defined to be 1.
(define level 1)

;; A set:er procedure used to update the level variable when a level is
;; completed.
(define (update-level!)
  (set! level (+ level 1)))

;; Add the escpae key to the hash table key-hash and give it the value #f.
(hash-set! key-hash 'escape #f)


;; -------------------------------- Utilities ----------------------------------


;; PROCEDURE: screen-wrap
;;
;; DESCIPTION: A procedure used to simulate that the screen
;;             "wraps around the edges", i.e. when an object, obj, passes
;;             outside of the screen on one side, it reappears at the opposite
;;             side when the middle of the object passes through the edge of
;;             the screen.
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


;; PROCEDURE: key-handler
;;
;; DESCRIPTION: When the key key-code is pressed/released it's corresponding
;;              value in key-hash is set to #t/#f. This means we can
;;              determine if several keys are pressed and handle every legal
;;              key combination.
;;
;; INPUT: key-code - a key code symbol.
;;        val - a boolean value, i.e. #t or #f.
;;
;; OUTPUT: #<void>
(define (key-handler key-code val)
  (hash-set! key-hash key-code val))


;; METHOD: collision?
;;
;; DESCRIPTION: A procedure that checks if the distance between the centers of
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
;; DESCRIPTION: A procedure responsible for drawing the values of some of the
;;              ship objects' fields on a drawing context, providing the player
;;              with information about the state of the game
;;
;; INPUT: dc - a drawing context.
;;
;; OUTPUT: #<void>
(define (draw-stats dc)
  (send dc set-text-foreground "white")
  (define xpos 10)
  (send dc draw-text (string-append "Level "
                                    (number->string level)) 960 10)
  (for-each (lambda (ship)
              (let ([name (get-field name ship)]
                    [score (number->string (send ship get-score))]
                    [lives (number->string (send ship get-lives))])
                (send dc draw-text name xpos 10)
                (send dc draw-text (string-append "Score: " score) xpos 30)
                (send dc draw-text (string-append "Lives: " lives) xpos 50)
                (set! xpos (+ xpos 200))))
            (hash-values ship-hash)))


