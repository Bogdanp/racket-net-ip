#lang racket/base

(require racket/contract/base
         racket/generic
         racket/stream
         "ip.rkt")

(provide gen:network

         (contract-out
          [network? (-> any/c boolean?)]
          [network-address (-> network? ip-address?)]
          [network-prefix (-> network? exact-nonnegative-integer?)]
          [network-hostmask (-> network? ip-address?)]
          [network-netmask (-> network? ip-address?)]
          [network-size (-> network? exact-nonnegative-integer?)]
          [network-member (-> network? ip-address? boolean?)]
          [network-version (-> network? (or/c 4 6))]
          [network->string (-> network? string?)]
          [network->stream (-> network? (stream/c ip-address?))]))

(define-generics network
  (network-address network)
  (network-prefix network)
  (network-hostmask network)
  (network-netmask network)
  (network-size network)
  (network-member network ip-address)
  (network-version network)
  (network->string network)
  (network->stream network))
