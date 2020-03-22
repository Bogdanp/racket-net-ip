#lang racket/base

(require racket/contract/base
         racket/format
         racket/match
         racket/stream
         racket/string
         racket/struct
         "address.rkt")

(provide
 (contract-out
  [make-network (case->
                 (-> (or/c ip-address? string?)
                     (or/c ip-address? string? exact-nonnegative-integer?)
                     network?)
                 (-> string? network?))]

  [network? (-> any/c boolean?)]
  [network (-> ip-address? exact-nonnegative-integer? network?)]
  [network-address (-> network? ip-address?)]
  [network-prefix (-> network? exact-nonnegative-integer?)]
  [network-hostmask (-> network? ip-address?)]
  [network-netmask (-> network? ip-address?)]
  [network-size (-> network? exact-nonnegative-integer?)]
  [network-member (-> network? ip-address? boolean?)]
  [network-version (-> network? (or/c 4 6))]
  [network->string (-> network? string?)]
  [network->stream (-> network? (stream/c ip-address?))]))

;; Generic IP networks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-network
  (match-lambda*
    [(list ip (and (? ip-address?) mask))
     (make-network ip (ip-address-bitcount mask))]

    [(list ip (and (? string?) mask))
     (make-network ip (ip-address-bitcount (make-ip-address mask)))]

    [(list (and (? ip-address?) ip)
           (and (? exact-nonnegative-integer?) prefix))
     (network ip prefix)]

    [(list (and (? string?) ip)
           (and (? exact-nonnegative-integer?) prefix))
     (network (make-ip-address ip) prefix)]

    [(list (and (? string?) net))
     (match (string-split net "/")
       [(list ip prefix)
        (network (make-ip-address ip)
                 (string->number prefix))]

       [_ (raise-argument-error 'make-network "a valid CIDR block" net)])]))

(struct network (address prefix)
  #:transparent
  #:guard
  (lambda (address prefix name)
    (unless (and (>= prefix 0)
                 (<= prefix (ip-address-size address)))
      (raise-argument-error 'network (format "a prefix value between 0 and ~a" (ip-address-size address)) prefix))

    (unless (zero? (bitwise-and (ip-address->number address)
                                (sub1 (expt 2 (- (ip-address-size address) prefix)))))
      (raise-arguments-error 'network "network has host bits set"
                             "address" address
                             "prefix" prefix))

    (values address prefix))

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (net) 'make-network)
      (lambda (net) (list (network->string net)))))])

(define (network-hostmask net)
  (make-ip-address (sub1 (network-size net))
                   (ip-address-version (network-address net))))

(define (network-netmask net)
  (make-ip-address (bitwise-xor (sub1 (expt 2 (ip-address-size (network-address net))))
                                (sub1 (network-size net)))
                   (ip-address-version (network-address net))))

(define (network-size net)
  (expt 2 (- (ip-address-size (network-address net))
             (network-prefix net))))

(define (network-member net addr)
  (= (ip-address->number (network-address net))
     (bitwise-and (ip-address->number (network-netmask net))
                  (ip-address->number addr))))

(define (network-version net)
  (ip-address-version (network-address net)))

(define (network->string net)
  (~a (ip-address->string (network-address net)) "/" (network-prefix net)))

(define (network->stream net)
  (for/stream ([i (in-range 0 (network-size net))])
    (ip-address-inc (network-address net) i)))
