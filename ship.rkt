#lang racket
(require racket/gui)
(provide ship%)

(define ship%
  (class object%
    (init-field [xpos 200]
                [ypos 200]
                [dx 0]
                [dy 0]
                [speed 0]
                [angle 0]
                [image (make-bitmap 100 100)])
    
    (define/public (get-image)
      image)
    
    (define/private (create-body-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-line
              50 0
              0 100)
        (send dc draw-line
              50 0
              100 100)
        (send dc draw-line
              0 99
              99 99)))
    
    (create-body-image image)
    
    (define/public (draw dc)
      (let ((w (send image get-width))
            (h (send image get-height)))
        
              (set! xpos (+ xpos dx))
              (set! ypos (+ ypos dy))
        
              (send dc translate (+ xpos (/ w 2)) (+ ypos (/ h 2)))
              (send dc rotate angle)
              (send dc draw-bitmap image (- (/ w 2)) (- (/ h 2)))
              (send dc rotate (- angle))
              (send dc translate (- (+ xpos (/ w 2))) (- (+ ypos (/ h 2))))
      
              (set! dx (* dx 0.99))
              (set! dy (* dy 0.99))))
    
        (define/public (key-handler key-event)
          (case (send key-event get-key-code)
            [(numpad8 #\w up) (set! speed (+ speed 1))
                              (set! dy (- dy (* speed (cos angle))))
                              (set! dx (- dx (* speed (sin angle))))]
            [(numpad4 #\a left) (set! angle (+ angle 0.18))]
            [(numpad2 #\d right) (set! angle (- angle 0.18))]))
        (super-new)))