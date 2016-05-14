#lang racket
(require racket/gui)
(provide game%)
(require "hash-tables.rkt")

;; game% is a class which handles the game's
;; physics, logic and drawing.
(define game%
  (class object%
    
    ;; screen-wrap is a method used to simulate
    ;; that the screen "wraps around the edges",
    ;; i.e. when an object, obj, passes outside
    ;; of the screen on one side, it reappears
    ;; at the opposite side when the middle of the object
    ;;passes through the edge.
    (define (screen-wrap obj)
      (let* ([xpos (send obj get-mid-xpos)]
             [ypos (send obj get-mid-ypos)])
        (cond
          [(> xpos 1920) (send obj set-mid-x! 0)]
          [(< xpos 0) (send obj set-mid-x! 1920)]
          [(> ypos 1080) (send obj set-mid-y! 0)]
          [(< ypos 0) (send obj set-mid-y! 1080)])))
    
    (define/public (key-handler key-code stat)
      (hash-set! key-hash key-code stat))
    
    ;;checks if two objects are closer to eachother than allowed
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
    
    (define (draw-stats dc)
      (send dc set-text-foreground "white")
      (define xpos 10)
      (for-each (lambda (ship)
                  (let ([score (number->string (send ship get-score))]
                        [lives (number->string (send ship get-lives))])
                    (send dc draw-text (string-append "Score " score) xpos 10)
                    (send dc draw-text (string-append "Lives " lives) xpos 30)
                    (set! xpos (+ xpos 200))))
                (hash-values ship-hash)))
    
    (define (end-of-level dc)
      (when (or (hash-empty? ship-hash)
                (hash-empty? asteroids-hash))
        (send dc set-text-foreground "white")
        (send dc draw-text "Game over!" 920 540)
        (send (new timer% [notify-callback exit]) start 3000)))
    
    ;; The acctual rendering method. Is called by the on-paint method
    ;; provided by the game-canvas% class. render just calls the update methods
    ;; above.
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
                    (send obj update dc)
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





