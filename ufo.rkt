#lang racket
(require racket/gui)
(require racket/random)
(require "hash-tables.rkt")
(require "bullet.rkt")
(provide ufo%)


(define ufo%
  (class object%
    
    (field [id 10] 
           [xpos 0] 
           [ypos 800] 
           [dx ((eval (random-ref '(+ -))) (random 1 5))]
           [points 200]
           [radius 75]
           [mid-xpos (+ xpos radius)]
           [mid-ypos (+ ypos radius)]
           [image (make-bitmap 150 150)]
           [name (gensym "ufo")])
    
    (super-new)
    
    (hash-set! ufo-hash name this)

    (define/public (get-mid-xpos)
      mid-xpos)

    (define/public (get-mid-ypos)
      mid-ypos)
    
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))
    
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (+ new-mid-y radius)))
    
    ;; Timer controlling when the ufo is to fire.
    (define *ufo-fire-timer*
      (new timer%
           [notify-callback (lambda ()
                              (unless (hash-empty? ship-hash)
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
             [target-x (send target get-mid-xpos)]
             [target-y (send target get-mid-ypos)]
             [target-angle (atan (/ (- target-y mid-ypos) (- target-x mid-xpos)))]
             [offset-x (* 75 (cos target-angle))]
             [offset-y (* 75 (sin target-angle))]
             [bullet-dx (* 25 (cos target-angle))]
             [bullet-dy (* 25 (sin target-angle))]
             [sign -])
        
        (when (> target-x mid-xpos)
          (set! sign +))
        
        (hash-set! bullets-hash bullet-name
                   (make-object bullet%
                     null
                     id
                     bullet-name
                     (sign mid-xpos offset-x)
                     (sign mid-ypos offset-y)
                     (sign bullet-dx)
                     (sign bullet-dy)))))
    
    (define/public (obj-has-collided-with obj)
      (destroy name))
    
    ;; When the ufo is to be destroyed we stop the ufo-fire-timer
    ;; and remove the ufo from the ufo-hash, meaning it wont be updated
    ;; anymore.
    (define (destroy name)
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
      (set! xpos (+ xpos dx))
      (set! mid-xpos (+ xpos radius))
      (set! mid-ypos (+ ypos radius)))))

