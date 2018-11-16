#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         racket/struct
         "common.rkt")

(provide (contract-out
          [make-ip-address (case->
                            (-> exact-nonnegative-integer? (or/c 4 6) ip-address?)
                            (-> (or/c string? (bytes-len/c 4) (bytes-len/c 16)) ip-address?))]

          [ip-address? (-> any/c boolean?)]
          [ip-address=? (-> ip-address? ip-address? boolean?)]
          [ip-address<? (-> ip-address? ip-address? boolean?)]
          [ip-address>? (-> ip-address? ip-address? boolean?)]
          [ip-address<=? (-> ip-address? ip-address? boolean?)]
          [ip-address>=? (-> ip-address? ip-address? boolean?)]
          [ip-address-bitcount (-> ip-address? exact-nonnegative-integer?)]
          [ip-address-size (-> ip-address? (or/c 32 128))]
          [ip-address-version (-> ip-address? (or/c 4 6))]
          [ip-address-dec (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address-inc (->* (ip-address?) (exact-integer?) ip-address?)]
          [ip-address->bytes (-> ip-address? bytes?)]
          [ip-address->number (-> ip-address? exact-nonnegative-integer?)]
          [ip-address->string (-> ip-address? string?)]

          [ipv4-address? (-> ip-address? boolean?)]
          [bytes->ipv4-address (-> bytes? ip-address?)]
          [number->ipv4-address (-> exact-nonnegative-integer? ip-address?)]
          [string->ipv4-address (-> string? ip-address?)]

          [ipv6-address? (-> ip-address? boolean?)]
          [bytes->ipv6-address (-> bytes? ip-address?)]
          [number->ipv6-address (-> exact-nonnegative-integer? ip-address?)]
          [string->ipv6-address (-> string? ip-address?)]))


;; Generic IP addresses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-ip-address
  (match-lambda*
    [(list (and (? exact-nonnegative-integer?) (var ip)) 4)
     (number->ipv4-address ip)]

    [(list (and (? exact-nonnegative-integer?) (var ip)) 6)
     (number->ipv6-address ip)]

    [(list (and (? ipv4-address-bytes?) (var ip)))
     (bytes->ipv4-address ip)]

    [(list (and (? ipv6-address-bytes?) (var ip)))
     (bytes->ipv6-address ip)]

    [(list (and (? string?) (var ip)))
     (if (string-contains? ip ":")
         (string->ipv6-address ip)
         (string->ipv4-address ip))]))

(define (fields->ip-address fields
                            #:version version
                            #:field-count [field-count (or (and (= version 4) 4) 8)]
                            #:field-size  [field-size  (or (and (= version 4) 8) 16)])
  (define value
    (for/fold ([v 0])
              ([f fields]
               [e (in-range (sub1 field-count) -1 -1)])
      (+ v (* f (expt (expt 2 field-size) e)))))

  (ip-address value version (* field-count field-size)))

(define (ip-address->fields addr field-count field-size)
  (define e (expt 2 field-size))
  (let loop ([n (ip-address-value addr)]
             [s '()])
    (cond
      [(= (length s) field-count) s]
      [else (loop (quotient n e)
                  (cons (remainder n e) s))])))

(struct ip-address (value version size)
  #:transparent
  #:guard
  (lambda (value version size name)
    (unless (and (>= value 0)
                 (<  value (expt 2 size)))
      (raise-argument-error 'ip-address (format "IPv~a addresses must be between 0 and 2^~a-1" version size) value))

    (values value version size))

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (addr) 'make-ip-address)
      (lambda (addr) (list (ip-address->string addr)))))])

(define-syntax-rule (define/comparison name fn)
  (define (name addr other)
    (unless (= (ip-address-version addr)
               (ip-address-version other))
      (raise-arguments-error (quote name)
                             "addresses are not of the same version"
                             "addr-a" addr
                             "addr-b" other))

    (fn (ip-address-value addr) (ip-address-value other))))

(define/comparison ip-address=? =)
(define/comparison ip-address<? <)
(define/comparison ip-address>? >)
(define/comparison ip-address<=? <=)
(define/comparison ip-address>=? >=)

(define (ip-address-bitcount addr)
  (bitcount (ip-address-value addr)))

(define (ip-address-dec addr [n 1])
  (struct-copy ip-address addr [value (- (ip-address-value addr) n)]))

(define (ip-address-inc addr [n 1])
  (struct-copy ip-address addr [value (+ (ip-address-value addr) n)]))

(define (ip-address->bytes addr)
  (apply bytes (match (ip-address-version addr)
                 [4 (ip-address->fields addr 4  8)]
                 [6 (ip-address->fields addr 16 8)])))

(define (ip-address->number addr)
  (ip-address-value addr))

(define (ip-address->string addr)
  (match (ip-address-version addr)
    [4 (ipv4-address->string addr)]
    [6 (ipv6-address->string addr)]))


;; IPv4 addresses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ipv4-address-bytes? bs)
  (and (bytes? bs) (= (bytes-length bs) 4)))

(define (ipv4-address? addr)
  (= (ip-address-version addr) 4))

(define (bytes->ipv4-address ip)
  (unless (= (bytes-length ip) 4)
    (raise-argument-error 'bytes->ipv4-address "exactly 4 bytes" ip))

  (fields->ip-address ip #:version 4))

(define (number->ipv4-address value)
  (ip-address value 4 32))

(define (string->ipv4-address ip)
  (define octets (map string->number (string-split ip ".")))
  (unless (andmap ipv4-octet? octets)
    (raise-argument-error 'string->ipv4-address "a valid IPv4 address" ip))

  (match octets
    [(list o1)          (fields->ip-address (list 0  0  0  o1) #:version 4)]
    [(list o1 o4)       (fields->ip-address (list o1 0  0  o4) #:version 4)]
    [(list o1 o2 o4)    (fields->ip-address (list o1 o2 0  o4) #:version 4)]
    [(list o1 o2 o3 o4) (fields->ip-address (list o1 o2 o3 o4) #:version 4)]

    [_ (raise-argument-error 'string->ipv4-address "4 decimal octets separated by dots" ip)]))

(define (ipv4-address->string addr)
  (string-join (map number->string (ip-address->fields addr 4 8)) "."))

(define (ipv4-octet? n)
  (and (>= n 0)
       (<= n 255)))


;; IPv6 addresses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ipv6-address-bytes? bs)
  (and (bytes? bs) (= (bytes-length bs) 16)))

(define (ipv6-address? addr)
  (= (ip-address-version addr) 6))

(define (bytes->ipv6-address ip)
  (unless (= (bytes-length ip) 16)
    (raise-argument-error 'bytes->ipv6-address "exactly 16 bytes" ip))

  (fields->ip-address ip
                      #:version 6
                      #:field-count 16
                      #:field-size 8))

(define (number->ipv6-address value)
  (ip-address value 6 128))

(define (string->ipv6-address ip)
  (match (string-split ip "::" #:trim? #f)
    [(list (and (app string->ipv6-fields fields)))

     (unless (= (length fields) 8)
       (raise-argument-error 'string->ipv6-adress "8 16-bit fields separated by colons" ip))

     (fields->ip-address fields #:version 6)]

    [(list (app string->ipv6-fields front)
           (app string->ipv6-fields back))

     (define field-count (+ (length front) (length back)))

     (unless (< field-count 8)
       (raise-argument-error 'string->ipv6-address "too many fields around :: pattern" ip))

     (define fields (append front (repeat 0 (- 8 field-count)) back))

     (fields->ip-address fields #:version 6)]

    [_ (raise-argument-error 'string->ipv6-adress "too many :: patterns" ip)]))

(define (string->ipv6-fields ip)
  (map string->ipv6-field (string-split ip ":" #:trim? #f)))

(define (string->ipv6-field f)
  (or (string->number f 16) 0))

(define (ipv6-address->string addr)
  (string-join (map number->ipv6-field (ip-address->fields addr 8 16)) ":"))

(define (number->ipv6-field f)
  (number->string f 16))
