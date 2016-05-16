#lang racket
(require racket/gui)
(provide ship%)
(require "bullet.rkt")
(require "hash-tables.rkt")

;; Purpose: ship% contains information for creating and handling
;; the input to ships.
;;
;; Last modified: 16-05-16
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.

(define ship%
  (class object%
    (init-field [id 1] ;; Id of the bullet. 
                [keys (list #\w #\a #\d #\space 'shift 'esc)]) ;; Key-bindings.
    (field [lives 3] ;; Ships lives.
           [xpos (random 1920)] ;; X-position fo the ship.
           [ypos (random 1080)] ;; y-position fo the ship.
           [angle 0] ;; Angle at which the ship is turned.
           [diameter 50] ;; Diameter of the ship.
           [radius (/ diameter 2)] ;; Radius of the ship.
           [mid-xpos (+ xpos radius)] ;; X-pos for ships middle
           [mid-ypos (+ ypos radius)] ;; Y-pos for ships middle
           [tip-xpos (- mid-xpos (* radius (sin angle)))] ;;X-pos for ships tip.
           [tip-ypos (- mid-ypos (* radius (cos angle)))] ;;Y-pos for ships tip.
           [dx 0] ;; Ship's speed in x-direction.
           [dy 0] ;; Ship's speed in y-direction.
           [speed 0] ;; Ship's velocity.
           [image (make-bitmap diameter diameter)] ;; Image of the ship.
           [points 300] ;; Number of points the object is worth.
           [score 0] ;; Player's score.
           [name (gensym "ship")]) ;; Gives the ship a unique name.
    
    (super-new)

    (create-ship-image image)
    
    
    ;; Adds the ship to the ship-hash
    (hash-set! ship-hash name this)
    
    (for-each (lambda (key)
                (hash-set! key-hash key #f))
              keys)
    
    ;; METHOD: get-image
    ;;
    ;; DESCRIPTION: Provides the image of the ship.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: image
    (define/public (get-image)
      image)
    
    ;; METHOD: get-score
    ;;
    ;; DESCRIPTION: Provides the score.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: score
    (define/public (get-score)
      score)
    
    ;; METHOD: update-score
    ;;
    ;; DESCRIPTION: Adds points the the score of the ship.
    ;;
    ;; INPUT: Ammount of points.
    ;;
    ;; OUTPUT: #<void>
    (define/public (update-score amt)
      (set! score (+ score amt)))
    
    ;; Method wich provides the number of remaining lives.
    (define/public (get-lives)
      lives)

    ;; Method: obj-has-collided-with
    ;;
    ;; DESCRIPTION: Changes the radius and therefore the hitbox of the ship
    ;; for 0,5s and sets it to a new position. If the ship is out of lives the
    ;; (destroy!) funktion is called.
    ;;
    ;; INPUT: a ship object.
    ;;
    ;; OUTPUT: #<void>
    (define/public (obj-has-collided-with obj)
      (let ([orig-radius radius])
        (set! radius -100)
        (send (new timer% [notify-callback
                           (lambda ()
                             (set! radius orig-radius))]) start 500)
        (set! xpos 920)
        (set! ypos 540)
        (if (= 0 lives)
            (destroy! name)
            (set! lives (- lives 1)))))
    
    ;; Method: destroy!
    ;;
    ;; DESCRIPOTION: Destroys the ship.
    ;;
    ;; INPUT: Name of ship.
    ;;
    ;; OUTPUT: #<void>
    (define (destroy! name)
      (hash-remove! ship-hash name))
    
    ;; METHOD: create-ship-image
    ;;
    ;; DESCRIPTION: A method wich draws a ship on a bitmap object.
    ;;
    ;; INPUT: bitmap-target
    ;;
    ;; OUTPUT: #<void>
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

    ;; METHOD: get-mid-x and get-mid-y
    ;;
    ;; DESCRIPTION: Provides the middle position of the ship.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: #<void>
    (define/public (get-mid-xpos)
      mid-xpos) 
    (define/public (get-mid-ypos)
      mid-ypos)

    ;; METHOD: set-mid-x! and set-mid-y!
    ;;
    ;; DESCRIPTION: Lets us set the position depending on the middle position.
    ;;
    ;; INPUT: New position for middle of the ship.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))  
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))
    
    ;; ------------- Move functions --------------

    ;; METHOD: accelerate
    ;;
    ;; DESCRIPTION: Increases the speed of the ship unless it's already moving
    ;; at the maximum speed.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: #<void>
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
    
    ;; METHOD: turn-left and turn-right
    ;
    ;; DESCRIPTION: Changes the direction in wich the ship is positioned.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT #<void>
    (define/private (turn-left)
      (set! angle (+ angle 0.085)))   
    (define/private (turn-right)
      (set! angle (- angle 0.085)))

    ;; METHOD: fire
    ;;
    ;; DESCRIPTION: Fires a bullet from the ship by creating instances
    ;; of the bullet% class.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: #<void>
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

    ;; METHOD: hyperdrive
    ;;
    ;; DESCRIPTION: Sets the position of the ship to a random position on the
    ;; screen.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: #<void>
    (define/private (hyperdrive)
      (set! xpos (random 1920))
      (set! ypos (random 1080)))

    ;; METHOD: steer
    ;;
    ;; DESCRIPTION: Controls the input and if they are correct runs the
    ;; different move-functions.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: #<void>
    (define (steer)
      (when (hash-ref key-hash (first keys))
        (accelerate))
      (when (hash-ref key-hash (second keys))
        (turn-left))
      (when (hash-ref key-hash (third keys))
        (turn-right))
      (when (hash-ref key-hash (fourth keys))
        (fire))
      (when (hash-ref key-hash (fifth keys))
        (hyperdrive)
        (hash-set! key-hash (fifth keys) #f)))
      
    
    
    ;; METHOD: update-ship
    ;;
    ;; DESCRIPTION: Calculates the ship's new position and renders it.
    ;;
    ;; INPUT: dc - drawing context
    ;;
    ;; OUTPUT: #<void>
    (define/public (update! dc)
      (let*  ([image-width (send image get-width)]
              [image-height (send image get-height)])
        (send dc translate (+ xpos (/ image-width 2))
              (+ ypos (/ image-height 2)))
        (send dc rotate angle)
        (send dc draw-bitmap image (- (/ image-width 2))
              (- (/ image-height 2)))
        (send dc rotate (- angle))
        (send dc translate (- (+ xpos (/ image-width 2)))
              (- (+ ypos (/ image-height 2))))
        (set! xpos (+ xpos dx))
        (set! ypos (+ ypos dy))
        (set! mid-xpos (+ xpos radius))
        (set! mid-ypos (+ ypos radius))
        (set! tip-xpos (- mid-xpos (* radius (sin angle))))
        (set! tip-ypos (- mid-ypos (* radius (cos angle))))
        (set! dx (* dx 0.98))
        (set! dy (* dy 0.98))
        (steer)))))

