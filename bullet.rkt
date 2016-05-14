#lang racket
(require racket/gui)
(provide bullet%)
(require "hash-tables.rkt")

(define bullet%
  (class object%
    (init-field
     [creator null]
     [id 3]
     [name null]
     [xpos 0]
     [ypos 0]
     [dx 0]
     [dy 0])
    
    (field [health 2]
           [diameter 8]
           [points 0]
           [radius (/ diameter 2)]
           [mid-xpos (+ xpos radius)]
           [mid-ypos (+ ypos radius)]
           [image (make-bitmap diameter diameter)])
    
    (super-new)
    
    ;;proveides the bitmap containing the bullet
    (define/public (get-image)
      image)
    
    (define/public (get-mid-xpos)
      mid-xpos)
    
    (define/public (get-mid-ypos)
      mid-ypos)
    
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))
    
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))
    
    (define/public (obj-has-collided-with obj)
      (unless (null? creator)
        (send creator update-score (get-field points obj)))
      (destroy name))
    
    (define/public (create-bullet-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc set-pen "white" 1 'solid)
        (send dc set-brush "black" 'solid)
        (send dc draw-ellipse 0 0 diameter diameter)))
    
    (create-bullet-image image)
    
    ;; destroys the bullet
    (define (destroy name)
      (hash-remove! bullets-hash name))
    
    ;; update-ship uses parameters provided by
    ;; bullet objects, which are stored in the bullet-hash,
    ;; calculates the bullets' new positions and draws them.
    ;; Currently, eh, unusable, to say the least...
    (define/public (update dc)
      
      ;; Drawing
      (send dc draw-bitmap image xpos ypos)
      
      ;; Logic
      (set! health (- health 0.05))
      
      (when (< health 0.001)
        (destroy name))
      
      ;; Physics
      (set! xpos (+ xpos dx))
      (set! ypos (+ ypos dy))
      (set! mid-xpos (+ xpos radius))
      (set! mid-ypos (+ ypos radius)))))