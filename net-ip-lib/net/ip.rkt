#lang racket/base

(require racket/contract/base
         racket/match
         "ip/ip.rkt"
         "ip/ipv4.rkt")

(provide (all-from-out "ip/ip.rkt")
         (all-from-out "ip/ipv4.rkt")
         (contract-out
          [make-ip-address (case->
                            (-> exact-nonnegative-integer? (or/c 4 16) ip-address?)
                            (-> (or/c string?
                                      ipv4-address-bytes?
                                      ipv6-address-bytes?) ip-address?))]))

(define (ipv4-address-bytes? bs)
  (and (bytes? bs) (= (bytes-length bs) 4)))

(define (ipv6-address-bytes? bs)
  (and (bytes? bs) (= (bytes-length bs) 16)))

(define make-ip-address
  (match-lambda*
    [(list (and (? exact-nonnegative-integer?) (var ip)) 4)
     (ipv4-address ip)]

    [(list (and (? ipv4-address-bytes?) (var ip)))
     (bytes->ipv4-address ip)]

    [(list (and (? string?) (var ip)))
     (string->ipv4-address ip)]))
