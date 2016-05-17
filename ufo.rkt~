#lang racket
(require racket/gui)
(require racket/random)
(require "hash-tables.rkt")
(require "bullet.rkt")
(provide ufo%)

;; PURPOSE:
;;
;; LAST MODIFIED:
;;
;; AUTHORS; Oscar Göransson and Edvin Ljungstrand.

(define ufo%
  (class object%
    
    (field [id 10] 
           [xpos 0] 
           [ypos 800] 
           [dx ((eval (random-ref '(+ -))) (random 1 5))]
           [points 200]
           [width 90]
           [height 50]
           [radius 50]
           [mid-xpos (+ xpos (/ width 2))]
           [mid-ypos (+ ypos (/ height 2))]
           [image (make-bitmap width (+ height 1))]
           [ufo-level level]
           [name (gensym "ufo")])
    
    (super-new)
    
    (hash-set! ufo-hash name this)
    
    
    ;; METHOD: get-mid-xpos
    ;;
    ;; DESCRIPTION: Method which returns the x-coordinate
    ;;              of the center of the ufo object.
    ;;
    ;; INPUT: None.
    ;;
    ;; OUTPUT: mid-xpos - the integer coordinate.
    (define/public (get-mid-xpos)
      mid-xpos)
    
    
    ;; METHOD: get-mid-ypos
    ;;
    ;; DESCRIPTION: Method which returns the x-coordinate
    ;;              of the center of the ufo object.
    ;;
    ;; INPUT: None.
    ;;
    ;; OUTPUT: mid-ypos - the integer coordinate.
    (define/public (get-mid-ypos)
      mid-ypos)
    
    
    ;; METHOD: set-mid-x!
    ;;
    ;; DESCRIPTION: Method which sets xpos, the x-coordinate of the center of
    ;;              the ufo object, to a new value, new-mid-x.
    ;;
    ;; INPUT: new-mid-x - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))
    
    
    ;; METHOD: set-mid-y!
    ;;
    ;; DESCRIPTION: Method which sets ypos, the y-coordinate of the center of
    ;;              the ufo object, to a new value, new-mid-y.
    ;;
    ;; INPUT: new-mid-y - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (+ new-mid-y radius)))

    
    ;; OBJECT: *ufo-fire-timer*
    ;;
    ;; DESCRIPTION: Timer controlling when the ufo is to fire.
    (define *ufo-fire-timer*
      (new timer%
           [notify-callback (lambda ()
                              (unless (hash-empty? ship-hash)
                                (fire)))]))
    
    
    ;; Start the ufo-fire-timer. We tell it to call the fire method
    ;; every 0.5 s.
    (send *ufo-fire-timer* start 2000)
    
    
    ;; METHOD: fire
    ;;
    ;; DESCRIPTION: The ufo's fire method. When the method is called it creates
    ;;               an instance of the bullet% class with initialization
    ;;               arguments provided so that the newly created bullet will
    ;;               travel toward a ship object. The initialization
    ;;               arguments depend on the level of the ufo in such a way that
    ;;               the accuracy of the ufo increases with it's level.
    ;; INPUT: None.
    ;;
    ;; OUTPUt: #<void>
    (define/public(fire)
      (let* ([bullet-name (gensym "bullet")]
             [target (random-ref (hash-values ship-hash))]
             [target-x (send target get-mid-xpos)]
             [target-y (send target get-mid-ypos)]
             [target-angle (atan (/ (- target-y mid-ypos) (- target-x mid-xpos)))]
             [offset-x (* 45 (cos target-angle))]
             [offset-y (* 45 (sin target-angle))]
             [bullet-dx (* 20 (cos target-angle))]
             [bullet-dy (* 20 (sin target-angle))]
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
    

    ;; METHOD: obj-has-collided-with
    ;;
    ;; DESCRIPTION: A method which determines what happens when an ufo object
    ;;              has been found to have collided with another object. On
    ;;              collision ufo's are destroyed, so the destroy method is
    ;;              called.
    ;;
    ;; INPUT: obj - an ufo object.
    ;;
    ;; OUTPUT: #<void>
    (define/public (obj-has-collided-with obj)
      (destroy! name))
    
    
    ;; METHOD : destroy
    ;;
    ;; DESCRIPTION: Mehtod specifying what is to happen when an ufo object is
    ;;              destroyed. When the ufo is to be destroyed the
    ;;              *ufo-fire-timer* is stopped and the ufo is removed from the
    ;;              ufo-hash, meaning it wont be updated anymore.
    ;;
    ;; INPUT: name - a string which serves as the key of the ufo object in
    ;;        the hash table ufo-hash.
    ;;
    ;; OUTPUT: #<void>
    (define (destroy! name)
      (send *ufo-fire-timer* stop)
      (hash-remove! ufo-hash name))


    ;; METHOD: create-ufo-image
    ;;
    ;; DESCRIPTION: A method which draws an "ufo shape" on a bitmap object,
    ;;              bitmap-target.
    ;;
    ;; INPUT: bitmap-target - a bitmap object.
    ;;
    ;; OUTPUT: #<void>
    (define/private (create-ufo-image bitmap-target)
      (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
        (send dc set-brush "black" 'solid)
        (send dc set-pen "white" 1 'solid)
        
        (send dc draw-line
              0 (* (/ 2 3) height)
              width (* (/ 2 3) height))
        (send dc draw-line
              0 (* (/ 2 3) height)
              (/ width 3) (/ height 3))
        (send dc draw-line
              width (* (/ 2 3) height)
              (- width (/ width 3)) (/ height 3))
        (send dc draw-line
              (/ width 3) (/ height 3)
              (- width (/ width 3)) (/ height 3))
        (send dc draw-line
              (/ width 3) (/ height 3)
              (+ (/ width 3) (/ width 12)) 0)
        (send dc draw-line
              (- width (/ width 3)) (/ height 3)
              (- width (/ width 3) (/ width 12)) 0)
        (send dc draw-line
              (+ (/ width 3) (/ width 12)) 0
              (- width (/ width 3) (/ width 12)) 0)
        (send dc draw-line
              0 (* (/ 2 3) height)
              (/ width 3) height)
        (send dc draw-line
              width (* (/ 2 3) height)
              (- width (/ width 3)) height)
        (send dc draw-line
              (/ width 3) height
              (- width (/ width 3)) height)))
    
    (create-ufo-image image)


    ;; METHOD: update
    ;;
    ;; DESCRIPTION: Draws a bitmap objet, image, on a drawing context at the
    ;;              (xpos ypos) location and then calculates new values for
    ;;              xpos, mid-xpos and mid-ypos. Only the x-coordinate is
    ;;              updated since ufo's only move in the x-direction. However
    ;;              for the sake of the screen-wrap method in game% the mid-ypos
    ;;              field is still updated.
    ;;
    ;; INPUT: dc - a drawing context.
    ;;
    ;; OUTPUT: #<void>
    (define/public (update! dc)
      (send dc draw-bitmap image xpos ypos)
      (set! xpos (+ xpos dx))
      (set! mid-xpos (+ xpos radius))
      (set! mid-ypos (+ ypos radius)))))

