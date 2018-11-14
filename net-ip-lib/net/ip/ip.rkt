#lang racket/base

(require racket/contract/base
         racket/generic)

(provide gen:ip-address

         (contract-out
          [ip-address? (-> any/c boolean?)]
          [ip-address-dec (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address-inc (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address->bytes (-> ip-address? bytes?)]
          [ip-address->number (-> ip-address? number?)]
          [ip-address->string (-> ip-address? string?)]
          [ip-address->version (-> ip-address? number?)]))

(define-generics ip-address
  (ip-address-dec ip-address [n])
  (ip-address-inc ip-address [n])
  (ip-address->bytes ip-address)
  (ip-address->number ip-address)
  (ip-address->string ip-address)
  (ip-address->version ip-address))
