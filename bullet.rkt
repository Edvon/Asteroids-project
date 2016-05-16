#lang racket
(require racket/gui)
(provide bullet%)
(require "hash-tables.rkt")

;; Purpose: Defines a bullet in our game and its properties.
;;
;; Last modified: 16-05-16
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.

(define bullet%
  (class object%
    (init-field
     [creator null] ;; Object of the creator of the bullet??????????????
     [id 3]         ;; Id of the bullet 
     [name null]    ;; Name of the bullet
     [xpos 0]       ;; X-position fo the bullet
     [ypos 0]       ;; Y-position för the bullet
     [dx 0]         ;; Speed in x-direction
     [dy 0])        ;; Speed in y-direction
    
    (field [health 2]     ;; Health of the bullet
           [diameter 8]   ;; Diameter of the bullet
           [points 0]     ;; Points of the bullet
           [radius (/ diameter 2)]      ;; Radius of the bullet
           [mid-xpos (+ xpos radius)]   ;; X-position of the middle for bullet
           [mid-ypos (+ ypos radius)]   ;; Y-position of the middle for bullet
           [image (make-bitmap diameter diameter)]) ;; Image of the bullet
    
    (super-new)

    (create-bullet-image image)
    
    ;; METHOD: get-image
    ;;
    ;; DESCRIPTION: Provides the image of the bullet.
    ;;
    ;; INPUT: none
    ;;
    ;; OUTPUT: image
    (define/public (get-image)
      image)
    
    ;; METHOD: get-mid-x and get-mid-y
    ;;
    ;; DESCRIPTION: Provides the middle position of the bullet.
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
    ;; INPUT: New position for middle of the bullet.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))  
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))

    ;; METHOD: obj-has-collided-with
    ;;
    ;; DESCRIPTION: When bullet has collided the creator of the bullets
    ;; score is updated with the points for the object wich has collided
    ;; with the bullet and the bullet is then deleted.
    ;;
    ;; INPUT: Colliding object
    ;;
    ;; OUTPUT: #<void>
    (define/public (obj-has-collided-with obj)
      (unless (null? creator)
        (send creator update-score (get-field points obj)))
      (destroy! name))

    ;; METHOD: create-bullet-image
    ;;
    ;; DESCRIPTION: A method wich draws a bullet on a bitmap object.
    ;;
    ;; INPUT: bitmap-target
    ;;
    ;; OUTPUT: #<void>
    (define/private (create-bullet-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc set-pen "white" 1 'solid)
        (send dc set-brush "black" 'solid)
        (send dc draw-ellipse 0 0 diameter diameter)))

    ;; METHOD: destroy!
    ;;
    ;; DESCRIPTION: Destroys the bullet by removeing it from its hash-list.
    ;;
    ;; INPUT: Name of the bullet wich will be destroyed.
    ;;
    ;; OUTPUT: #<void>
    (define (destroy! name)
      (hash-remove! bullets-hash name))

    ;; METHOD: update
    ;;
    ;; DESCRIPTION: Parameters provided by bullet objects, which are stored in
    ;; the bullet-hash, calculates the bullets' new positions and draws them.
    ;;
    ;; INPUT: dc - drawing context
    ;;
    ;; Output: #<void>  
    (define/public (update! dc)
      (send dc draw-bitmap image xpos ypos)
      (set! health (- health 0.05))    
      (when (< health 0.001)
        (destroy! name))
      (set! xpos (+ xpos dx))
      (set! ypos (+ ypos dy))
      (set! mid-xpos (+ xpos radius))
      (set! mid-ypos (+ ypos radius)))))