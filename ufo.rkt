#lang racket
(require racket/gui)
(require racket/random)
(require "hash-tables.rkt")
(require "bullet.rkt")
(provide ufo%)


(define ufo%
  (class object%
    (init-field [id 10] 
                [xpos 0] 
                [ypos 800] 
                [dx ((eval (random-ref '(+ -))) (random 1 5))]
                [points 200]
                [image (make-bitmap 150 150)]
                [name (gensym "ufo")])
    
    (hash-set! ufo-hash name this)
    
    (define/public (mid-x)
      (+ xpos 75))
    (define/public (mid-y)
      (+ ypos 75))
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x 75)))
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (+ new-mid-y 75)))
    
    (define/public (radius)
      50)

    ;; Timer controlling when the ufo is to fire.
    (define *ufo-fire-timer*
      (new timer%
           [notify-callback (lambda ()
                              (unless (null? (hash-values ship-hash))
                                (fire)))]))
    
    ;; Start the ufo-fire-timer. We tell it to call the fire method
    ;; every 0.5 s.
    (send *ufo-fire-timer* start 2000)


    ;; Working on integrating the position of ships into
    ;; the fire method. The aim is to make ufos fire toward
    ;; the ships.
    (define/private (fire)
      (let* ([bullet-name (gensym "bullet")]
             [target (random-ref (hash-values ship-hash))]
             [target-x (send target mid-x)]
             [target-y (send target mid-y)]
             [target-angle (atan (/ (- target-y ypos) (- target-x xpos)))]
             [offset-x (* 75 (cos target-angle))]
             [offset-y (* 75 (sin target-angle))]
             [bullet-dx (* 25 (cos target-angle))]
             [bullet-dy (* 25 (sin target-angle))]
             [sign -])
        
        (when (> target-x xpos)
          (set! sign +))
        
        (hash-set! bullets-hash bullet-name
                   (make-object bullet%
                     (sign (mid-x) offset-x)
                     (sign (mid-y) offset-y)
                     (sign bullet-dx)
                     (sign bullet-dy)
                     bullet-name
                     id))))

    ;; When the ufo is to be destroyed we stop the ufo-fire-timer
    ;; and remove the ufo from the ufo-hash, meaning it wont be updated
    ;; anymore.
    (define/public (destroy name)
      (send *ufo-fire-timer* stop)
      (hash-remove! ufo-hash name))
    
    (define/public (create-ufo-image bitmap-target)
      (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
        (send dc set-brush "black" 'solid)
        (send dc set-pen "white" 1 'solid)
        (send dc draw-ellipse 0 0 150 150)
        (send dc draw-ellipse 50 50 50 50)))
    
    (create-ufo-image image)
    
    (define/public (update dc)
      
      ;; Drawing
      (send dc draw-bitmap image xpos ypos)
      
      
      ;; Physics
      (set! xpos (+ xpos dx)))
    
    (super-new)))

