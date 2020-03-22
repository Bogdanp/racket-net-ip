#lang racket/base

(require racket/contract/base
         racket/math)

(provide
 bitcount
 bytes-len/c
 repeat)

(define (bitcount n [size (or (and (= 0 n) 0)
                              (sub1 (exact-ceiling (log n 2))))])
  (for/fold ([c 0])
            ([i (in-range size -1 -1)])
    #:break (not (bitwise-bit-set? n i))
    (add1 c)))

(define (bytes-len/c n)
  (flat-named-contract
   `(bytes-len/c ,n)
   (lambda (bs)
     (and (bytes? bs)
          (= (bytes-length bs) n)))))

(define (repeat x n)
  (for/fold ([xs '()])
            ([_ (in-range n)])
    (cons x xs)))
