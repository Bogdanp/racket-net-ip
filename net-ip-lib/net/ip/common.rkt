#lang racket/base

(require racket/contract/base
         racket/math)

(provide bitcount
         repeat)

(define (bitcount n [size (sub1 (exact-ceiling (log n 2)))])
  (for/fold ([c 0])
            ([i (in-range size -1 -1)])
    #:break (not (bitwise-bit-set? n i))
    (add1 c)))

(define (repeat x n)
  (for/fold ([xs '()])
            ([_ (in-range n)])
    (cons x xs)))
