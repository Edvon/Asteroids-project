#lang racket
(require racket/gui)
(provide game%)

;; game% is a class which handles the game's
;; physics, logic and drawing.
(define game%
  (class object%
    
    ;; screen-wrap is a method used to simulate
    ;; that the screen "wraps around the edges",
    ;; i.e. when an object, obj, passes outside
    ;; of the screen on one side, it reappears
    ;; at the opposite side when the middle of the object
    ;;passes through the edge.
    (define (screen-wrap obj)
      (let* ([xpos (send obj mid-x)]
             [ypos (send obj mid-y)])
        (cond
          [(> xpos 1920) (send obj set-mid-x! 0)]
          [(< xpos 0) (send obj set-mid-x! 1920)]
          [(> ypos 1080) (send obj set-mid-y! 0)]
          [(< ypos 0) (send obj set-mid-y! 1080)])))
    
    ;; update-ship uses parameters provided by the
    ;; ship object, calculates the ship's new position
    ;; and renders it.
    (define (update-ship ship dc)
      (let* ([image (send ship get-image)]
             [image-width (send image get-width)]
             [image-height (send image get-height)]
             [xpos (get-field xpos ship)]
             [ypos (get-field ypos ship)]
             [dx (get-field dx ship)]
             [dy (get-field dy ship)]
             [angle (get-field angle ship)]
             [speed (get-field speed ship)])
        
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
        (set! dx (* dx 0.99))
        (set! dy (* dy 0.99))
        (set-field! xpos ship xpos)
        (set-field! ypos ship ypos)
        (set-field! dx ship dx)
        (set-field! dy ship dy)
        (screen-wrap ship)))
    
    ;; update-ship uses parameters provided by
    ;; bullet objects, which are stored in the bullet-hash,
    ;; calculates the bullets' new positions and draws them.
    ;; Currently, eh, unusable, to say the least...
    (define (update-bullets bullet-hash dc)
      (for-each (lambda (bullet)
                  (let*  ([bullet-name (get-field name bullet)]
                          [health (get-field health bullet)]
                          [image (send bullet get-image)]
                          [image-width (send image get-width)]
                          [image-height (send image get-height)]
                          [angle (get-field angle bullet)]
                          [xpos  (get-field xpos bullet)]
                          [ypos (get-field ypos bullet)]
                          [dx (get-field dx bullet)]
                          [dy (get-field dy bullet)])
                    
                    ;; Drawing
                    (send dc draw-bitmap image xpos ypos)
                    
                    ;; Logic
                    (set! health (- health 0.005))
                    (set-field! health bullet health)
                    (when (< health 0.001)
                      (send bullet destroy-bullet bullet-name))
                    
                    ;; Physics
                    (set! xpos (+ xpos dx))
                    (set! ypos (+ ypos dy))
                    (set-field! xpos bullet xpos)
                    (set-field! ypos bullet ypos)
                    (screen-wrap bullet)))
                (hash-values bullet-hash)))
    
    ;; update-asteroids uses parameters provided by
    ;; asteroid objects, which are stored in the asteroid-hash,
    ;; calculates the asteroids' new positions and draws them.
    (define (update-asteroids asteroids-hash dc)
      (for-each (lambda (asteroid)
                  (let* ([asteroid-name (get-field name asteroid)]
                         [health (get-field health asteroid)]
                         [image (send asteroid get-image)]
                         [image-width (send image get-width)]
                         [image-height (send image get-height)]
                         [xpos  (get-field xpos asteroid)]
                         [ypos (get-field ypos asteroid)]
                         [dx (get-field dx asteroid)]
                         [dy (get-field dy asteroid)])
                    
                    ;; Drawing
                    (send dc draw-bitmap image xpos ypos)

                     
                    ;; Logic
                    (set! health (- health 0.005))
                    (set-field! health asteroid health)
                    (when (< health 0.001)
                      (send asteroid destroy-asteroid asteroid-name))
                    
                    
                    ;; Physics
                    (set! xpos (+ xpos dx))
                    (set! ypos (+ ypos dy))
                    
                    (set-field! xpos asteroid xpos)
                    (set-field! ypos asteroid ypos)
                    (set-field! dy asteroid dy)
                    (set-field! dx asteroid dx)
                    (screen-wrap asteroid)))
                (hash-values asteroids-hash)))
    
    ;; The acctual rendering method. Is called by the on-paint method
    ;; provided by the game-canvas% class. render just calls the update methods
    ;; above.
    (define/public (render ship bullet-hash asteroids-hash dc)
      (update-ship ship dc)
      (update-asteroids asteroids-hash dc)
      (update-bullets bullet-hash dc))
    (super-new)))