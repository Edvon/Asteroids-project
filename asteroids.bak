#lang racket
(require racket/gui)
(require racket/random)
(provide asteroid%)
(provide medium-asteroid%)
(provide asteroids-hash)

;; Create a hash table, asteroids-hash, in which all the asteroid-objects
;; will be stored. We use hash tables to store stuff throughout the project
;; because they're mutable, and it turns out to be rather convenient.
(define asteroids-hash (make-hash))


;; asteroid% contains the information for creating asteroids
;; at random locations, thus all the use of the random procedure.
(define asteroid%
  (class object%
    (init-field [xpos (random 1920)] ;; The asteroid's current x-coordinate. Is updated whenever the screen in refreshed.
                [ypos (random 1080)] ;; The asteroid's current y-coordinate. Is updated whenever the screen in refreshed.
                [diameter 200] ;; The diameter of the circle representing the asteroid.
                [dx ((eval (random-ref '(+ -))) (random 1 4))] ;; The asteroid's speed in the x-direction. A number between -4 and 4.
                [dy ((eval (random-ref '(+ -))) (random 1 4))] ;; The asteroid's speed in the y-direction. A number between -4 and 4.
                [health 2] ;; The asteroid's health.
                [name (gensym "asteroid")]) ;; A randomly generated name.
    
    ;; Method for generating a bitmap.
    (define image (make-bitmap diameter diameter))
    
    (define/public (mid-x)
      (+ xpos 100))
    (define/public (mid-y)
      (+ ypos 100))
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x 100)))
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y 100)))
    
    ;; We add the astreoid to the hash-table asteroids-hash.
    (hash-set! asteroids-hash name this)
    
    ;; Method which provides us with the bitmap containing the asteroid.
    (define/public (get-image)
      image)
    
    ;; A method for destroying an asteroid and at the same time creating 2 new, smaller asteroids.
    ;; This creates the illusion, kind of, of the asteroid breaking into pieces.
    ;; We create the new asteroids in the vicinty of where the old asteroid was destroyed.
    (define/public (destroy-asteroid name)
      (for ([i 2])
        (make-object medium-asteroid% (+ xpos (* 20 i)) (+ ypos (* 20 i))))
      (hash-remove! asteroids-hash name))
    
    ;; A method which draws the asteroid on the bitmap.
    (define/public (create-asteroid-image bitmap-target)
      (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
        (send dc draw-ellipse 0 0 diameter diameter)))
    
    (create-asteroid-image image)
    (super-new)))

(define medium-asteroid%
  (class asteroid%
    (init-field [mxpos 0] ;; The x-coordinate on which the asteroid is to be created.
                [mypos 0]) ;; The y-coordinate on which the asteroid is to be created.
    
    ;; We create an instance of the asteroid%-class, only changing relevant fields.
    ;; We make the new asteroid smaller and weaker.
    (super-new [xpos mxpos]
               [ypos mypos]
               [diameter 100]
               [health 1]
               [name (gensym "medium-asteroid")])
    
    ;; We inherit xpos and ypos from our new instantiated object
    ;; and assign them to new variable names.
    (inherit-field [new-xpos xpos]
                   [new-ypos ypos])
    
    ;; We override the definition of the destroy-asteroid method to make it
    ;; create 3 even smaller asteroids. We supply the make-object call
    ;; with our inherited coordinates to, just as before, create the new asteroids
    ; close to where the old one disappeared.
    (define/override (destroy-asteroid name)
      (for ([i 3])
        (make-object small-asteroid% (+ new-xpos (* 10 i)) (+ new-ypos (* 10 i))))
      (hash-remove! asteroids-hash name))))

;; As before, only smaller and weaker...
(define small-asteroid%
  (class asteroid%
    (init-field [sxpos 0]
                [sypos 0])
    (super-new [xpos sxpos]
               [ypos sypos]
               [diameter 50]
               [health 0.5]
               [name (gensym "small-asteroid")])
    
    ;; The smallest asteroids don't create new asteroids when they're destroyed, but
    ;; simply vanish.
    (define/override (destroy-asteroid name)
      (hash-remove! asteroids-hash name))))

