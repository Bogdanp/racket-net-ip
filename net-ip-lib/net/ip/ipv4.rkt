#lang racket/base

(require racket/contract/base
         racket/format
         racket/match
         racket/stream
         racket/string
         racket/struct
         "ip.rkt"
         "network.rkt")

(provide (contract-out
          [ipv4-address? (-> any/c boolean?)]
          [ipv4-address (-> exact-nonnegative-integer? ipv4-address?)]
          [bytes->ipv4-address (-> bytes? ipv4-address?)]
          [string->ipv4-address (-> string? ipv4-address?)]

          [ipv4-network? (-> any/c boolean?)]
          [ipv4-network (-> ipv4-address? number? ipv4-network?)]
          [string->ipv4-network (-> string? ipv4-network?)]))


;; IPv4 addresses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MIN-IPV4-VALUE 0)
(define MAX-IPV4-VALUE (sub1 (expt 256 4)))

(define (ipv4-address=? a b recursive-equal?)
  (= (ipv4-address-value a)
     (ipv4-address-value b)))

(define (ipv4-address-hash-1 addr recursive-equal-hash?)
  (ipv4-address-value addr))

(define (ipv4-address-hash-2 addr recursive-equal-hash?)
  (ipv4-address-value addr))

(define (ipv4-address-octets addr)
  (let loop ([n (ipv4-address-value addr)]
             [s '()])
    (cond
      [(= (length s) 4) s]
      [else (loop (quotient n 256)
                  (cons (remainder n 256) s))])))

(struct ipv4-address (value)
  #:guard
  (lambda (value name)
    (unless (and (>= value MIN-IPV4-VALUE)
                 (<= value MAX-IPV4-VALUE))
      (raise-argument-error 'ipv4-address "IPv4 addresses must be between 0 and 256^4-1" value))
    value)

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (addr) 'string->ipv4-address)
      (lambda (addr) (list (ip-address->string addr)))))]

  #:methods gen:equal+hash
  [(define equal-proc ipv4-address=?)
   (define hash-proc  ipv4-address-hash-1)
   (define hash2-proc ipv4-address-hash-2)]

  #:methods gen:ip-address
  [(define (ip-address=? addr other)
     (equal? addr other))

   (define (ip-address<? addr other)
     (< (ipv4-address-value addr)
        (ipv4-address-value other)))

   (define (ip-address>? addr other)
     (> (ipv4-address-value addr)
        (ipv4-address-value other)))

   (define (ip-address<=? addr other)
     (<= (ipv4-address-value addr)
         (ipv4-address-value other)))

   (define (ip-address>=? addr other)
     (>= (ipv4-address-value addr)
         (ipv4-address-value other)))

   (define (ip-address-dec addr [n 1])
     (ipv4-address (- (ipv4-address-value addr) n)))

   (define (ip-address-inc addr [n 1])
     (ipv4-address (+ (ipv4-address-value addr) n)))

   (define (ip-address-version addr)
     4)

   (define (ip-address->bytes addr)
     (apply bytes (ipv4-address-octets addr)))

   (define (ip-address->number addr)
     (ipv4-address-value addr))

   (define (ip-address->string addr)
     (string-join (map number->string (ipv4-address-octets addr)) "."))])

(define (bytes->ipv4-address ip)
  (unless (= (bytes-length ip) 4)
    (raise-argument-error 'bytes->ipv4-address "exactly 4 bytes" ip))

  (apply make-ipv4 (bytes->list ip)))

(define (string->ipv4-address ip)
  (define octets (map string->number (string-split ip ".")))
  (unless (andmap ipv4-octet? octets)
    (raise-argument-error 'string->ipv4-address "a valid IPv4 address" ip))

  (match octets
    [(list o1)          (make-ipv4 0  0  0  o1)]
    [(list o1 o4)       (make-ipv4 o1 0  0  o4)]
    [(list o1 o2 o4)    (make-ipv4 o1 o2 0  o4)]
    [(list o1 o2 o3 o4) (make-ipv4 o1 o2 o3 o4)]

    [_ (raise-argument-error 'string->ipv4-address "4 octets separated by dots" ip)]))

(define (make-ipv4 o1 o2 o3 o4)
  (ipv4-address (+ o4
                   (* o3 256)
                   (* o2 (expt 256 2))
                   (* o1 (expt 256 3)))))

(define (ipv4-octet? n)
  (and (>= n 0)
       (<= n 255)))


;; IPv4 networks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MIN-IPV4-PREFIX 0)
(define MAX-IPV4-PREFIX 32)

(define (ipv4-network=? a b recursive-equal?)
  (and (= (ipv4-network-prefix a)
          (ipv4-network-prefix b))
       (equal? (ipv4-network-address a)
               (ipv4-network-address b))))

(define (ipv4-network-hash net recursive-equal-hash?)
  (+ (* (ipv4-address-hash-1 (ipv4-network-address net)) 1)
     (* (ipv4-network-prefix net) (expt 256 4))))

(struct ipv4-network (address prefix)
  #:guard
  (lambda (address prefix name)
    (unless (ipv4-address? address)
      (raise-argument-error 'ipv4-network "an ipv4-address" address))

    (unless (and (>= prefix MIN-IPV4-PREFIX)
                 (<= prefix MAX-IPV4-PREFIX))
      (raise-argument-error 'ipv4-network "a prefix value between 0 and 32" prefix))

    (when (not (= 0 (bitwise-and (ip-address->number address)
                                 (sub1 (expt 2 (- 32 prefix))))))
      (raise-arguments-error 'ipv4-network "network has host bits set"
                             "address" (ip-address->string address)
                             "prefix" prefix))

    (values address prefix))

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (net) 'string->ipv4-network)
      (lambda (net) (list (network->string net)))))]

  #:methods gen:equal+hash
  [(define equal-proc ipv4-network=?)
   (define hash-proc  ipv4-network-hash)
   (define hash2-proc ipv4-network-hash)]

  #:methods gen:network
  [(define (network-address net)
     (ipv4-network-address net))

   (define (network-hostmask net)
     (ipv4-address (sub1 (network-size net))))

   (define (network-netmask net)
     (ipv4-address (bitwise-xor #xFFFFFFFF (sub1 (network-size net)))))

   (define (network-size net)
     (expt 2 (- 32 (ipv4-network-prefix net))))

   (define (network-member net addr)
     (= 0 (bitwise-xor (ip-address->number (ipv4-network-address net))
                       (bitwise-and (ip-address->number (network-netmask net))
                                    (ip-address->number addr)))))

   (define (network-version net)
     4)

   (define (network->string net)
     (~a (ip-address->string (ipv4-network-address net)) "/" (ipv4-network-prefix net)))

   (define (network->stream net)
     (for/stream ([i (in-range 0 (network-size net))])
       (ip-address-inc (network-address net) i)))])

(define (string->ipv4-network net)
  (match (string-split net "/")
    [(list ip p)
     (ipv4-network (string->ipv4-address ip)
                   (string->number p))]

    [_ (raise-argument-error 'string->ipv4-network "a valid CIDR block" net)]))
