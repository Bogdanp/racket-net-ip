#lang racket/base

(require racket/contract/base
         racket/match
         "ip/ip.rkt"
         "ip/ipv4.rkt"
         "ip/network.rkt")

(provide (all-from-out "ip/ip.rkt")
         (all-from-out "ip/ipv4.rkt")
         (all-from-out "ip/network.rkt")
         (contract-out
          [make-ip-address (case->
                            (-> exact-nonnegative-integer? 4 ip-address?)
                            (-> (or/c string? ipv4-address-bytes?) ip-address?))]
          [make-network (case->
                         (-> (or/c string? ipv4-address?) exact-nonnegative-integer? network?)
                         (-> string? network?))]))

(define (ipv4-address-bytes? bs)
  (and (bytes? bs) (= (bytes-length bs) 4)))

(define make-ip-address
  (match-lambda*
    [(list (and (? exact-nonnegative-integer?) (var ip)) 4)
     (ipv4-address ip)]

    [(list (and (? ipv4-address-bytes?) (var ip)))
     (bytes->ipv4-address ip)]

    [(list (and (? string?) (var ip)))
     (string->ipv4-address ip)]))

(define make-network
  (match-lambda*
    [(list (and (? ipv4-address?) (var ip))
           (and (? exact-nonnegative-integer?) (var prefix)))
     (ipv4-network ip prefix)]

    [(list (and (? string?) (var ip))
           (and (? exact-nonnegative-integer?) (var prefix)))
     (ipv4-network (string->ipv4-address ip) prefix)]

    [(list (and (? string?) (var net)))
     (string->ipv4-network net)]))
