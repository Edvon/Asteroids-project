#lang racket
(require racket/gui)
(provide ship%)
(require "bullet.rkt")
(require "utilities.rkt")

;; Purpose: ship% contains information for creating ship objects and handling
;;          their input and logic.
;;
;; Last modified: 16-05-25
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.

(define ship%
  (class object%
    
    
    ;;[field]         [comment]
    
    ;;[name]          [Name of the ship]
    ;;[keys]          [Key-bindings.]
    ;;[id])           [Id of the ship.] 
    
    ;;[lives]         [Ship's lives.]
    ;;[xpos]          [The bitmap's x-coordinate.]
    ;;[ypos]          [The bitmap's y-coordinate.]
    ;;[angle]         [Angle at which the ship is turned.]
    ;;[diameter]      [The height and width of the ship's bitmap.]
    ;;[radius]        [diameter / 2]
    ;;[mid-xpos]      [x-coordinate for the middle of the ship.]
    ;;[mid-ypos]      [y-coordinate for the middle of the ship.]
    ;;[tip-xpos]      [x-coordinate for the tip of the ship.]
    ;;[tip-ypos]      [y-coordinate for tip of the ship.]
    ;;[dx]            [Ship's speed in the x-direction.]
    ;;[dy]            [Ship's speed in the y-direction.]
    ;;[speed]         [Ship's velocity.]
    ;;[image]         [The ship's bitmap.]
    ;;[points]        [Number of points the object is worth.]
    ;;[score]         [Player's score.]
    ;;[fire-cooldown] [Cooldown for fire procedure.]
    
    (init-field [name "Player 1"]
                [keys (list #\w #\a #\d #\space 'shift)]
                [id 1])               
    (field [lives 3] 
           [xpos (random 1920)] 
           [ypos (random 1080)] 
           [angle 0] 
           [diameter 50] 
           [radius (/ diameter 2)] 
           [mid-xpos (+ xpos radius)] 
           [mid-ypos (+ ypos radius)]
           [tip-xpos (- mid-xpos (* radius (sin angle)))] 
           [tip-ypos (- mid-ypos (* radius (cos angle)))] 
           [dx 0] 
           [dy 0] 
           [speed 0] 
           [image (make-bitmap diameter diameter)] 
           [points 300] 
           [score 0] 
           [fire-cooldown 0]) 
    
    
    (super-new)
    
    (create-ship-image image)
    
    
    ;; Adds the ship to the ship-hash
    (hash-set! ship-hash name this)
    
    ;; Set every element in keys to false in key-hash, since no key is pressed
    ;; on object creation.
    (for-each (lambda (key)
                (hash-set! key-hash key #f))
              keys)
    
    ;; METHOD: get-image
    ;;
    ;; DESCRIPTION: Provides the field image.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: image - a bitmap object.
    (define/public (get-image)
      image)
    
    ;; METHOD: get-score
    ;;
    ;; DESCRIPTION: Provides the field score.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: score - an integer.
    (define/public (get-score)
      score)
    
    ;; METHOD: update-score
    ;;
    ;; DESCRIPTION: Adds amnt points the ship's score.
    ;;
    ;; INPUT: amt - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (update-score amnt)
      (set! score (+ score amnt)))
    
    ;; Method which provides the lives field.
    (define/public (get-lives)
      lives)
    
    ;; Method: obj-has-collided-with
    ;;
    ;; DESCRIPTION: Changes the radius and therefore the hitbox of the ship
    ;; for 0.5 s and sets it to a new position. If the ship is out of lives the
    ;; (destroy!) method is called.
    ;;
    ;; INPUT: obj -  a ship object.
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
    ;; DESCRIPOTION: Destroys the ship by removing it from ship-hash, meaning it
    ;; won't get updated nor rendered, and adds it to the dead-ship-hash for
    ;; storing,
    ;;
    ;; INPUT: name - a string.
    ;;
    ;; OUTPUT: #<void>
    (define/public (destroy! name)
      (hash-set! dead-ship-hash name this)
      (hash-remove! ship-hash name))
    
    ;; METHOD: create-ship-image
    ;;
    ;; DESCRIPTION: Draws a ship on the bitmap bitmap-target.
    ;;
    ;; INPUT: bitmap-target - a bitmap object.
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
    
    ;; METHOD: get-mid-x
    ;;
    ;; DESCRIPTION: Provides the x-coordinate of the middle of the ship.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: mid-xpos - a number (probably a float).
    (define/public (get-mid-xpos)
      mid-xpos)
    
    ;; METHOD: get-mid-y
    ;;
    ;; DESCRIPTION: Provides the y-coordinate of the middle of the ship.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: mid-ypos - a number (probably a float).
    (define/public (get-mid-ypos)
      mid-ypos)
    
    ;; METHOD: set-mid-x!
    ;;
    ;; DESCRIPTION: When the object has been given a new value for it's mid-xpos
    ;;              field set-mid-x! calculates and sets a new value for the
    ;;              xpos field.
    ;;            
    ;; INPUT: new-mid-x - a number (probably a float).
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))
    
    ;; METHOD: set-mid-y!
    ;;
    ;; DESCRIPTION: When the object has been given a new value for it's mid-ypos
    ;;              field set-mid-y! calculates and sets a new value for the
    ;;              ypos field.
    ;;            
    ;; INPUT: new-mid-y - a number (probably a float).
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))
    
    
    ;; ------------------------------ Move -------------------------------------
    
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
        (when (= fire-cooldown 0)
          (set! fire-cooldown 10)
          (hash-set! bullets-hash bullet-name
                     (make-object bullet%
                       this
                       id
                       bullet-name
                       tip-xpos
                       tip-ypos
                       (- (* 20 (sin angle)))
                       (- (* 20 (cos angle))))))))
    
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
    ;; DESCRIPTION: Checks if any of the keys in the keys-field have been
    ;;              pressed and if so calls the appropriate methods.
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
    
    ;; -------------------------------------------------------------------------
    
    ;; METHOD: update-ship
    ;;
    ;; DESCRIPTION: Calculates the ship's new position, updates fields and
    ;;              renders the ship. The ship bitmap is translated so that the
    ;;              center of the bitmap becomes the origo of it's
    ;;              coordinate-system, then it's rotated angle radians around
    ;;              the new origo, drawn and translated back.
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
        (when (> fire-cooldown 0)
          (set! fire-cooldown (- fire-cooldown 1)))
        (steer)))))

