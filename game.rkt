#lang racket
(require racket/gui)
(provide game%)

(define game%
  (class object%
    (init-field obj)

    (field [image (send obj get-image)])
    
    (define/public (render dc)
        (send obj draw dc))
    
    (super-new)))