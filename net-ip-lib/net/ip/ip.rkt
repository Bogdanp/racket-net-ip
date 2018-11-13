#lang racket/base

(require racket/contract/base
         racket/generic)

(provide gen:ip-address

         (contract-out
          [ip-address? (-> any/c boolean?)]
          [ip-address-dec (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address-inc (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address->string (-> ip-address? string?)]))

(define-generics ip-address
  (ip-address-dec ip-address [n])
  (ip-address-inc ip-address [n])
  (ip-address->string ip-address))
