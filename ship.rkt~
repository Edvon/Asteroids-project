#lang racket
(require racket/gui)
(provide ship%)
(require "bullet.rkt")

;; ship% contains information for creating and handling
;; the input to ships.
(define ship%
  (class object%
    (init-field [xpos 200] ;The ship's x-coordinate.
                [ypos 200] ;The ship's y-coordinate.
                [dx 0] ;The ship's speed in the x-direction.
                [dy 0] ;The ship's speed in the y-direction.
                [speed 0] ;The velocity.
                [angle (/ pi 4)] ;The angle at which the ship is turned.
                [image (make-bitmap 100 100)]) ;A bitmap to draw the ship in.
    
    
    ;; Method which provides the bitmap.
    (define/public (get-image)
      image)
    
    ;; Method for drawing the ship in the bitmap.
    (define/private (create-ship-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-line
              50 0
              0 100)
        (send dc draw-line
              50 0
              100 100)
        (send dc draw-line
              0 100
              50 75)
        (send dc draw-line
              100 100
              50 75)))
    
    ;; The x and y coordinates for middle of the ship bitmap and for the
    ;;tip of the ship.
    
    (define/public (mid-x)
      (+ xpos 50))
    (define/public (mid-y)
      (+ ypos 50))
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x 50)))
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y 50)))
    
    (define/public (tip-xpos)
      (- (send this mid-x) (* 50 (sin angle))))
    (define/public (tip-ypos)
      (- (send this mid-y) (* 50 (cos angle))))
    
    
    ;; update-ship uses parameters provided by the
    ;; ship object, calculates the ship's new position
    ;; and renders it.
    (define/public (update-ship dc)
      (let*  ([image-width (send image get-width)]
              [image-height (send image get-height)])
        
        ;; Drawing
        (send dc translate (+ xpos (/ image-width 2))
              (+ ypos (/ image-height 2)))
        (send dc rotate angle)
        (send dc draw-bitmap image (- (/ image-width 2))
              (- (/ image-height 2)))
        (send dc rotate (- angle))
        (send dc translate (- (+ xpos (/ image-width 2)))
              (- (+ ypos (/ image-height 2))))
        
        ;; Physics
        (set! xpos (+ xpos dx))
        (set! ypos (+ ypos dy))
        (set! dx (* dx 0.99))
        (set! dy (* dy 0.99))))
        
        
        ;; Currently out of order... The idea is to create instances of the bullet%
        ;; class when the ship fires, add these to a hash table and then update
        ;; everything in the hash table by using game%. We need to find som expresion
        ;; so that bullets are created at the front of the ship, irregardless of the
        ;; ship's direction.
        (define/private (fire)
          (let ([bullet-name (gensym "bullet")])
            (hash-set! bullet-hash bullet-name
                       (make-object bullet% 1
                         (send this tip-xpos) (send this tip-ypos) (- (* 20 (sin angle))) (- (* 20 (cos angle))) bullet-name))))
        
        
        ;; Method for handling key-events. When the 'w' key is pressed we increase
        ;; the speed of the ship, unless the ship is going to fast already, and
        ;; determine the value of the speed in both the x and y-direction.
        ;; When either'a' or 'd' are pressed we turn the ship.
        (define/public (move key-code)
          (case key-code
            [(#\w) (cond ((> speed 1)
                          (set! speed 1))
                         (else
                          (set! speed (+ speed 0.35))
                          (set! dy (- dy (* speed (cos angle))))
                          (set! dx (- dx (* speed (sin angle))))))]
            [(#\a) (set! angle (+ angle 0.2))]
            [(#\d) (set! angle (- angle 0.2))]
            [(#\space) (fire)]))
        
        (create-ship-image image)
        
        (super-new)))