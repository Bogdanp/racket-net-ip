#lang racket/base

(require racket/contract/base
         "ip/ip.rkt"
         "ip/ipv4.rkt")

(provide (all-from-out "ip/ip.rkt")
         (all-from-out "ip/ipv4.rkt")
         (contract-out
          [make-ip-address (-> string? ip-address?)]))

(define (make-ip-address ip)
  (parse-ipv4 ip))
