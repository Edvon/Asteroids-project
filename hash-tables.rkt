#lang racket
(provide (all-defined-out))

(define key-hash (make-hash))

(define ship-hash (make-hash))

;; Create a hash table, asteroids-hash, in which all the asteroid-objects
;; will be stored. We use hash tables to store stuff throughout the project
;; because they're mutable, and it turns out to be rather convenient.
(define asteroids-hash (make-hash))

(define score 0)

(define (update-score amt)
  (set! score (+ score amt)))

(define bullets-hash (make-hash))

(define ufo-hash (make-hash))

