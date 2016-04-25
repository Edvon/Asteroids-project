#lang racket
(require racket/gui)
(provide asteroid%)

(define asteroid%
  (class object%
    [init-field [image (make-bitmap 100 100)]
                [x 0]
                [y 0]
                [dx 0]
                [dy 0]
                [speed 1]
                [angle (random 7)]]))






(define (asteroid-image bitmap-target)
  ((let ((dc (new bitmap-dc% [bitmap bitmap-target])))
    (send dc set-brush (make-object brush% "orange" 'solid))
    (send dc draw-ellipse 10 10 60 60))))
    


       