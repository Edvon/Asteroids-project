#lang racket
(require racket/gui)
(provide bullet%)
(provide bullets-hash)
;; Very much work in progress. Nothing of worth here...

(define bullets-hash (make-hash))

(define bullet%
  (class object%
    (init-field 
                [xpos 0]
                [ypos 0]
                [dx 5]
                [dy 5]
                [name ""]
                [id 3]
                [angle 0]
                [health 0.25]
                [image (make-bitmap 10 10)])               
    
    ;;proveides the bitmap containing the bullet
    (define/public (get-image)
      image)
    
    ;;the x and y coordinets in the middle of the bitmap.
    (define/public (mid-x)
      (+ xpos 5))
    (define/public (mid-y)
      (+ ypos 5))
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x 5)))
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y 5)))

    ;;provides the radius of the bullet
    (define/public (radius)
      5)
    
    
    (define/private (create-bullet-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-ellipse 0 0 10 10)))
    
    (create-bullet-image image)

    ;; destroys the bullet
    (define/public (destroy name)
      (hash-remove! bullets-hash name))
    
    ;; update-ship uses parameters provided by
    ;; bullet objects, which are stored in the bullet-hash,
    ;; calculates the bullets' new positions and draws them.
    ;; Currently, eh, unusable, to say the least...
    (define/public (update dc)
      
      ;; Drawing
      (send dc draw-bitmap image xpos ypos)
      
      ;; Logic
      (set! health (- health 0.005))

      (when (< health 0.001)
        (destroy name))
      
      ;; Physics
      (set! xpos (+ xpos dx))
      (set! ypos (+ ypos dy)))
    (super-new)))