#lang racket
(require racket/gui)
(provide bullet%)
(provide bullet-hash)

;; Very much work in progress. Nothing of worth here...

(define bullet-hash (make-hash))

(define bullet%
  (class object%
    (init-field [health 1]
                [xpos 0]
                [ypos 0]
                [dx 5]
                [dy 5]
                [name ""]
                [angle 0]
                [image (make-bitmap 10 10)])
    
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
    
    (define/public (destroy-bullet name)
      (hash-remove! bullet-hash name))
    
    (define/private (create-bullet-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-ellipse 0 0 10 10)))
    
    (create-bullet-image image)
    
    (super-new)))