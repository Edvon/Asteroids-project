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
                [keys (list #\w #\a #\d #\space 'shift)])
    
    
    (field [lives 3] ;When the ship is out of lives it should be removed.
           [xpos (random 1920)] ;The ship's x-coordinate.
           [ypos (random 1080)] ;The ship's y-coordinate.
           [angle 0] ;The angle at which the ship is turned.
           [diameter 50]
           [radius (/ diameter 2)] ;The radius of the object's hit-box.
           [mid-xpos (+ xpos radius)]
           [mid-ypos (+ ypos radius)]
           [tip-xpos (- mid-xpos (* radius (sin angle)))]
           [tip-ypos (- mid-ypos (* radius (cos angle)))]
           [dx 0] ;The ship's speed in the x-direction.
           [dy 0] ;The ship's speed in the y-direction.
           [speed 0] ;The velocity.
           [image (make-bitmap diameter diameter)] ;A bitmap to draw the ship in.
           [points 300] ;The number of points the object is worth.
           [score 0] ;The player's score.
           [name (gensym "ship")]) ;;Gives the ship a unique name
    
    (super-new)
    
    
    ;; adds the ship to the ship-hash
    (hash-set! ship-hash name this)
    
    (for-each (lambda (key)
                (hash-set! key-hash key #f))
              keys)
    
    ;; Method which provides the bitmap.
    (define/public (get-image)
      image)
    
    (define/public (get-score)
      score)
    
    (define/public (update-score amt)
      (set! score (+ score amt)))
    
    (define/public (get-lives)
      lives)
    
    (define/public (obj-has-collided-with obj)
      (let ([orig-radius radius])
        (set! radius -100)
        (send (new timer% [notify-callback (lambda ()
                                             (set! radius orig-radius))]) start 500)
        (set! xpos 920)
        (set! ypos 540)
        (if (= 0 lives)
            (destroy name)
            (set! lives (- lives 1)))))
    
    ;; destroys the ship when out of health
    (define (destroy name)
      (hash-remove! ship-hash name))
    
    
    ;; Method for drawing the ship in the bitmap.
    (define/private (create-ship-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc set-brush "black" 'solid)
        (send dc set-pen "white" 1 'solid)
        (send dc draw-line
              radius 0
              0 diameter)
        (send dc draw-line
              radius 0
              diameter diameter)
        (send dc draw-line
              0 diameter
              radius (- diameter (/ radius 2)))
        (send dc draw-line
              diameter diameter
              radius (- diameter (/ radius 2)))))
    
    (define/public (get-mid-xpos)
      mid-xpos)
    
    (define/public (get-mid-ypos)
      mid-ypos)
    
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))
    
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))
    
    ;;move functions
    
    ;; forward increases the speed of the ship unless it's already moving
    ;; at the maximum speed.
    (define/private (accelerate)
      (if (>= speed 0.5)
          (begin
            (set! speed 0.5)
            (set! dy (- dy (* speed (cos angle))))
            (set! dx (- dx (* speed (sin angle)))))
          (begin
            (set! speed (+ speed 0.02))
            (set! dy (- dy (* speed (cos angle))))
            (set! dx (- dx (* speed (sin angle)))))))
    
    ;; Turn-left and turn-right changes the direction in wich the ship is moving.
    (define/private (turn-left)
      (set! angle (+ angle 0.085)))
    
    (define/private (turn-right)
      (set! angle (- angle 0.085)))
    
    ;; create instances of the bullet%
    ;; class when the ship fires, add these to a hash table and then update
    ;; everything in the hash table by using game%.
    (define/private (fire)
      (let ([bullet-name (gensym "bullet")])
        (hash-set! bullets-hash bullet-name
                   (make-object bullet%
                     this
                     id
                     bullet-name
                     tip-xpos
                     tip-ypos
                     (- (* 20 (sin angle)))
                     (- (* 20 (cos angle)))))))
    
    (define/private (hyperdrive)
      (set! xpos (random 1920))
      (set! ypos (random 1080)))
    
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
        (fire)
        (hash-set! key-hash (fourth keys) #f))
      (when (hash-ref key-hash (fifth keys))
        (hyperdrive)
        (hash-set! key-hash (fifth keys) #f)))
    
    
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
        (set! mid-xpos (+ xpos radius))
        (set! mid-ypos (+ ypos radius))
        (set! tip-xpos (- mid-xpos (* radius (sin angle))))
        (set! tip-ypos (- mid-ypos (* radius (cos angle))))
        (set! dx (* dx 0.98))
        (set! dy (* dy 0.98))
        
        ;;steer
        (steer)))
    
    (create-ship-image image)))

