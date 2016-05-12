#lang racket
(require racket/gui)
(provide game%)
(require "hash-tables.rkt")

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
    
    (define/public (key-handler key-code stat)
      (case key-code
        [(equal? key-code #\w) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\a) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\d) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\space) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\i) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\j) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\l) (hash-set! key-hash key-code stat)]
        [(equal? key-code #\m) (hash-set! key-hash key-code stat)]))
    
    ;;checks if two objects are closer to eachother than allowed
    (define (collision? obj-1 obj-2)
      (<=
       (sqrt (+ (sqr (- (send obj-1 mid-x) (send obj-2 mid-x)))
                (sqr (- (send obj-1 mid-y) (send obj-2 mid-y)))))
       (+ (send obj-1 radius) (send obj-2 radius))))
    
    
    ;; The acctual rendering method. Is called by the on-paint method
    ;; provided by the game-canvas% class. render just calls the update methods
    ;; above.
    (define/public (render dc)
      (let* ((all-obj-lst (append (hash-values ship-hash)
                                  (hash-values bullets-hash)
                                  (hash-values asteroids-hash))))
        (for-each (lambda (obj)
                    (send obj update dc)
                    (screen-wrap obj))
                  all-obj-lst)
        (for-each (lambda (obj1)
                    (for-each (lambda (obj2)
                                (unless (equal? (get-field id obj1)
                                                (get-field id obj2))
                                  (when (collision? obj1 obj2)
                                    (send obj2 destroy (get-field name obj2)))))
                              all-obj-lst))
                  all-obj-lst)))
    (super-new)))





