#lang racket
(require racket/gui)
(provide ship%)
(require "bullet.rkt")
(require "hash-tables.rkt")

;; ship% contains information for creating and handling
;; the input to ships.
(define ship%
  (class object%
    (init-field [id 1] ;;The identification-number för the ship
                [keys (list #\w #\a #\d #\space)]
                [xpos (random 1920)] ;The ship's x-coordinate.
                [ypos (random 1080)] ;The ship's y-coordinate.
                [dx 0] ;The ship's speed in the x-direction.
                [dy 0] ;The ship's speed in the y-direction.
                [speed 0] ;The velocity.
                [angle (/ pi 4)] ;The angle at which the ship is turned.
                [health 4] ;When the ship is out of life it should be removed.
                [cooldown 5] ;
                [image (make-bitmap 100 100)] ;A bitmap to draw the ship in.    
                [name (gensym "ship")]) ;;Gives the ship a unique name
    
    
    ;; adds the ship to the ship-hash
    (hash-set! ship-hash name this)
    
    (for-each (lambda (key)
                (hash-set! key-hash key #f))
              keys)
    
    ;; Method which provides the bitmap.
    (define/public (get-image)
      image)
    
    ;; destroys the ship when out of health
    (define/public (destroy name)
      (if (= 0 health)
      (hash-remove! ship-hash name)
      (set! health (- health 1))))
    
    ;; Method for drawing the ship in the bitmap.
    (define/private (create-ship-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc set-brush "black" 'solid)
        (send dc set-pen "white" 1 'solid)
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
    
    ;;provides the radius of the ship
    (define/public (radius)
      45)
    
    ;;move functions
    
    ;; forward increases the speed of the ship unless it's already moving
    ;; at the maximum speed.
    (define/private (accelerate)
      (if (>= speed 1)
          (begin
            (set! speed 1)
            (set! dy (- dy (* speed (cos angle))))
            (set! dx (- dx (* speed (sin angle)))))
          (begin
            (set! speed (+ speed 0.08))
            (set! dy (- dy (* speed (cos angle))))
            (set! dx (- dx (* speed (sin angle)))))))
    
    ;; Turn-left and turn-right changes the direction in wich the ship is moving.
    
    (define/private (turn-left)
      (set! angle (+ angle 0.05)))
    
    (define/private (turn-right)
      (set! angle (- angle 0.05)))
     
    ;; create instances of the bullet%
    ;; class when the ship fires, add these to a hash table and then update
    ;; everything in the hash table by using game%.
    (define/private (fire)
      (let ([bullet-name (gensym "bullet")])
        (hash-set! bullets-hash bullet-name
                   (make-object bullet% 
                     (tip-xpos)
                     (tip-ypos)
                     (- (* 15 (sin angle)))
                     (- (* 15 (cos angle)))
                     bullet-name
                     id))))
   
    ;; function wich controls the input and if they are correct runs the
    ;; different move-functions.
    (define (steer)
      (when (hash-ref key-hash (first keys))
        (accelerate))
      (when (hash-ref key-hash (second keys))
        (turn-left))
      (when (hash-ref key-hash (third keys))
        (turn-right))
      (when (hash-ref key-hash (fourth keys))
        (fire)))
    
    
    ;; update-ship uses parameters provided by the
    ;; ship object, calculates the ship's new position
    ;; and renders it.
    (define/public (update dc)
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
        (set! dx (* dx 0.97))
        (set! dy (* dy 0.97))
        
        ;;steer
        (steer)))
    
    (create-ship-image image)
    
    (super-new)))

