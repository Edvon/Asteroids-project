#lang racket
(require racket/gui)
(require racket/random)
(provide asteroid%)
(provide medium-asteroid%)
(require "utilities.rkt")

;; Purpose: asteroid% contains the information for creating asteroids,
;;          handling their movement and their logic.
;;
;; Last modified: 16-05-25
;;
;; AUTHORS: Oscar Göransson and Edvin Ljungstrand.

(define asteroid%
  (class object%

    ;;[field]         [comment]

    ;;[xpos]          [The bitmap's x-coordinate.]
    ;;[ypos]          [The bitmap's y-coordinate.]
    ;;[diameter]      [The height and width of the asteroids's bitmap.]
    ;;[points]        [Number of points the object is worth.]
    ;;[name]          [Name of the asteroid.]
    ;;                ((gensym "asteroid") Gives the asteroid a unique name.)
    ;;[dx]            [Asteroid's speed in x-direction.]
    ;;                (A random number between -4 and 4.)
    ;;[dy]            [Asteroid's speed in y-direction.]
    ;;                (A random number between -4 and 4.)
    ;;[id])           [Id of the asteroid.]  
    ;;[radius]        [diameter / 2]
    ;;[mid-xpos]      [x-coordinate for the middle of the asteroid.]
    ;;[mid-ypos]      [y-coordinate for the middle of the asteroid.]  

    (init-field [xpos (random 1920)] 
                [ypos (random 1080)] 
                [diameter 150] 
                [points 50] 
                [name (gensym "asteroid")] 
                [dx ((eval (random-ref '(+ -))) (random 1 3))] 
                [dy ((eval (random-ref '(+ -))) (random 1 3))]) 
    
    
    (field [id 4]
           [radius (/ diameter 2)]
           [mid-xpos (+ xpos radius)]
           [mid-ypos (+ ypos radius)])
    
    (super-new)
    
    ;; We add the astreoid to the hash-table asteroids-hash.
    (hash-set! asteroids-hash name this)
    
    ;; Define a image to be a quadratic bitmap.
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
    ;; DESCRIPTION: When the object has been given a new value for it's mid-xpos
    ;;              field set-mid-x! calculates and sets a new value for the
    ;;              xpos field.
    ;;            
    ;; INPUT: new-mid-x - an integer.
    ;;
    ;; OUTPUT: #<void>
    (define/public (set-mid-x! new-mid-x)
      (set! xpos (- new-mid-x radius)))


    ;; METHOD: set-mid-y!
    ;;
    ;; DESCRIPTION: When the object has been given a new value for it's mid-ypos
    ;;              field set-mid-y! calculates and sets a new value for the
    ;;              ypos field.
    ;;            
    ;; INPUT: new-mid-y - an integer.
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



;;---------------------------- Medium asteroids---------------------------------


(define medium-asteroid%
  (class asteroid%

    ;; We create an instance of the asteroid%-class, only changing relevant
    ;; fields. We make the new asteroid smaller, weaker and faster.
    
    ;;[field]        [comment]
    
    ;;[mxpos]        [The x-coordinate on which the asteroid is to be created.]
    ;;[mypos]        [The y-coordinate on which the asteroid is to be created.]
    
    (init-field [mxpos 0] 
                [mypos 0]) 
              
    (super-new [xpos mxpos]
               [ypos mypos]
               [diameter 85]
               [points 75]
               [name (gensym "medium-asteroid")]
               [dx ((eval (random-ref '(+ -))) (random 2 5))]
               [dy ((eval (random-ref '(+ -))) (random 2 5))]) 
    
    ;; We inherit xpos and ypos from our new instantiated object
    ;; and assign them to new variable names.
    (inherit-field [new-xpos xpos]
                   [new-ypos ypos])

    ;; As before.
    (define/override (update! dc)
      (super update! dc))
    
    ;; We override the definition of the destroy-asteroid method to make it
    ;; create 3 even smaller asteroids. We supply the make-object call
    ;; with our inherited coordinates to, just as before, create the new
    ;; asteroids close to where the old one disappeared.
    (define/override (destroy! name)
      (for ([i 2])
        (make-object small-asteroid% (+ new-xpos (* 10 i)) (+ new-ypos (* 10 i))))
      (hash-remove! asteroids-hash name))))


;; ---------------------------- Small asteroid ---------------------------------

;; As before; only smaller, faster and worth more points.
(define small-asteroid%
  (class asteroid%
    (init-field [sxpos 0]
                [sypos 0])
    (super-new [xpos sxpos]
               [ypos sypos]
               [diameter 45]
               [points 100]
               [name (gensym "small-asteroid")]
               [dx ((eval (random-ref '(+ -))) (random 4 7))]
               [dy ((eval (random-ref '(+ -))) (random 4 7))])
    
    ;; As before.
    (define/override-final (update! dc)
      (super update! dc))
    
    ;; The smallest asteroids don't create new asteroids when they're destroyed,
    ;; but simply vanish.
    (define/override-final (destroy! name)
      (hash-remove! asteroids-hash name))))

