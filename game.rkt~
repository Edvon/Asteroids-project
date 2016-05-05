#lang racket
(require racket/gui)
(provide game%)

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
      (let* ([xpos (send obj mid-x)]
             [ypos (send obj mid-y)])
        (cond
          [(> xpos 1920) (send obj set-mid-x! 0)]
          [(< xpos 0) (send obj set-mid-x! 1920)]
          [(> ypos 1080) (send obj set-mid-y! 0)]
          [(< ypos 0) (send obj set-mid-y! 1080)])))
    
    
    
    
    ;; The acctual rendering method. Is called by the on-paint method
    ;; provided by the game-canvas% class. render just calls the update methods
    ;; above.
    (define/public (render ship bullet-hash asteroids-hash dc)
      (send ship update-ship dc)
      (screen-wrap ship)
      (for-each (lambda (asteroid)
                  (send asteroid update-asteroid dc)
                  (screen-wrap asteroid))
                (hash-values asteroids-hash))
      (for-each (lambda (bullet)
                  (send bullet update-bullet dc)
                  (screen-wrap bullet))
                (hash-values bullet-hash)))
      (super-new)))