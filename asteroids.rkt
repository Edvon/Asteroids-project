#lang racket
(require racket/gui)
(require racket/random)
(provide asteroid%)
(provide medium-asteroid%)
(require "hash-tables.rkt")


;; asteroid% contains the information for creating asteroids
;; at random locations, thus all the use of the random procedure.
(define asteroid%
  (class object%
    (init-field [xpos (random 1920)] ;; The asteroid's current x-coordinate.
                [ypos (random 1080)] ;; The asteroid's current y-coordinate.
                [diameter 150] ;; The diameter of the circle representing the asteroid.
                [points 50] 
                [name (gensym "asteroid")]) ;; A randomly generated name.
    
    
    (field [id 4]
           [radius (/ diameter 2)]
           [mid-xpos (+ xpos radius)]
           [mid-ypos (+ ypos radius)]
           [dx ((eval (random-ref '(+ -))) (random 1 4))] ;; The asteroid's speed in the x-direction. A number between -4 and 4.
           [dy ((eval (random-ref '(+ -))) (random 1 4))]) ;; The asteroid's speed in the y-direction. A number between -4 and 4.)
    
    (super-new)
    
    ;; We add the astreoid to the hash-table asteroids-hash.
    (hash-set! asteroids-hash name this)
    
    ;; Method for generating a bitmap.
    (define image (make-bitmap diameter diameter))
    

    ;; METHOD: get-image
    ;;
    ;; DESCRIPTION: A method which returns the bitmap of the asteroid object.
    ;;
    ;; INPUT: None.
    ;;
    ;; OUTPUT: image - a bitmap object.
    (define/public (get-image)
      image)
    
    
    ;; METHOD: create-asteroid-image
    ;;
    ;; DESCRIPTION: A method which draws a circle on a bitmap object,
    ;;              bitmap-target.
    ;;
    ;; INPUT: bitmap-target - a bitmap object.
    ;;
    ;; OUTPUT: #<void>
    (define/private (create-asteroid-image bitmap-target)
      (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
        (send dc set-brush "black" 'solid)
        (send dc set-pen "white" 1 'solid)
        (send dc draw-ellipse 0 0 diameter diameter)))
    
    
    ;; METHOD: get-mid-xpos
    ;;
    ;; DESCRIPTION: Method which returns the x-coordinate
    ;;              of the center of the asteroid object.
    ;;
    ;; INPUT: None.
    ;;
    ;; OUTPUT: mid-xpos - the integer coordinate.
    (define/public (get-mid-xpos)
      mid-xpos)
    
    
    ;; METHOD: get-mid-ypos
    ;;
    ;; DESCRIPTION: Method which returns the y-coordinate
    ;;              of the center of the asteroid object.
    ;;
    ;; INPUT: None.
    ;;
    ;; OUTPUT: mid-ypos - the integer coordinate.
    (define/public (get-mid-ypos)
      mid-ypos)


    ;; METHOD: set-mid-x!
    ;;
    ;; DESCRIPTION: Method which sets xpos, the x-coordinate of the center of
    ;;              the asteroid object, to a new value, new-mid-x.
    ;;
    ;; INPUT: new-mid-x - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))


    ;; METHOD: set-mid-x!
    ;;
    ;; DESCRIPTION: Method which sets ypos, the x-coordinate of the center of
    ;;              the asteroid object, to a new value, new-mid-x.
    ;;
    ;; INPUT: new-mid-x - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-y! new-mid-y)
      (set! ypos (- new-mid-y radius)))
    
    (define/public (obj-has-collided-with obj)
      (destroy! name))
    
    
    ;; METHOD: destroy!
    ;;
    ;; DESCRIPTION: A method for destroying an asteroid and at the same time
    ;;              creating two new, smaller asteroids in the vicinty of where
    ;;              the old asteroid was destroyed. This creates the illusion
    ;;              of the asteroid breaking into pieces.
    ;;
    ;; INPUT: name - a string which serves as the key of the asteroid object in
    ;;        the hash table asteroids-hash.
    ;;
    ;; OUTPUT: #<void>
    (define/public (destroy! name)
      (for ([i 2])
        (make-object medium-asteroid% (+ xpos (* 20 i)) (+ ypos (* 20 i))))
      (hash-remove! asteroids-hash name))
    
    
    ;; METHOD: update
    ;;
    ;; DESCRIPTION: Draws a bitmap objet, image, on a drawing context at the
    ;;              (xpos ypos) location and then calculates new values for
    ;;              xpos, ypos, mid-xpos and mid-ypos.
    ;;
    ;; INPUT: dc - a drawing-context.
    ;;
    ;; OUTPUT: #<void>
    (define/public (update! dc)
      (send dc draw-bitmap image xpos ypos)
      (set! xpos (+ xpos dx))
      (set! ypos (+ ypos dy))
      (set! mid-xpos (+ xpos radius))
      (set! mid-ypos (+ ypos radius)))
    
    (create-asteroid-image image)))

(define medium-asteroid%
  (class asteroid%
    (init-field [mxpos 0] ;; The x-coordinate on which the asteroid is to be created.
                [mypos 0]) ;; The y-coordinate on which the asteroid is to be created.
    
    ;; We create an instance of the asteroid%-class, only changing relevant fields.
    ;; We make the new asteroid smaller and weaker.
    (super-new [xpos mxpos]
               [ypos mypos]
               [diameter 85]
               [points 75]
               [name (gensym "medium-asteroid")])
    
    ;; We inherit xpos and ypos from our new instantiated object
    ;; and assign them to new variable names.
    (inherit-field [new-xpos xpos]
                   [new-ypos ypos])
    
    (define/override (update! dc)
      (super update! dc))
    
    ;; We override the definition of the destroy-asteroid method to make it
    ;; create 3 even smaller asteroids. We supply the make-object call
    ;; with our inherited coordinates to, just as before, create the new asteroids
    ; close to where the old one disappeared.
    (define/override (destroy! name)
      (for ([i 2])
        (make-object small-asteroid% (+ new-xpos (* 10 i)) (+ new-ypos (* 10 i))))
      (hash-remove! asteroids-hash name))))

;; As before, only smaller and weaker...
(define small-asteroid%
  (class asteroid%
    (init-field [sxpos 0]
                [sypos 0])
    (super-new [xpos sxpos]
               [ypos sypos]
               [diameter 45]
               [points 150]
               [name (gensym "small-asteroid")])
    
    (define/override-final (update! dc)
      (super update! dc))
    
    ;; The smallest asteroids don't create new asteroids when they're destroyed, but
    ;; simply vanish.
    (define/override-final (destroy! name)
      (hash-remove! asteroids-hash name))))

