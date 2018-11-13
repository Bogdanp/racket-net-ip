#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         "ip.rkt")

(provide (contract-out
          [struct ipv4-address ((value exact-integer?))]
          [parse-ipv4 (-> string? ipv4-address?)]))

(define MIN-IPV4-VALUE 0)
(define MAX-IPV4-VALUE (sub1 (expt 256 4)))

(struct ipv4-address (value)
  #:transparent

  #:guard
  (lambda (value name)
    (unless (and (>= value MIN-IPV4-VALUE)
                 (<= value MAX-IPV4-VALUE))
      (raise-argument-error 'ipv4-address "IPv4 addresses must be between 0 and 256^4-1" value))
    value)

  #:methods gen:ip-address
  [(define (ip-address-dec addr [n 1])
     (ipv4-address (- (ipv4-address-value addr) n)))

   (define (ip-address-inc addr [n 1])
     (ipv4-address (+ (ipv4-address-value addr) n)))

   (define (ip-address->string addr)
     (let loop ([n (ipv4-address-value addr)]
                [s '()])
       (if (= 4 (length s))
           (string-join (map number->string s) ".")
           (loop (quotient n 256)
                 (cons (remainder n 256) s)))))])

(define (parse-ipv4 ip)
  (define octets (map string->number (string-split ip ".")))
  (unless (andmap ipv4-octet? octets)
    (raise-argument-error 'parse-ipv4 "a valid IPv4 address" ip))

  (match octets
    [(list o1)
     (make-ipv4 0 0 0 o1)]

    [(list o1 o4)
     (make-ipv4 o1 0 0 o4)]

    [(list o1 o2 o4)
     (make-ipv4 o1 o2 0 o4)]

    [(list o1 o2 o3 o4)
     (make-ipv4 o1 o2 o3 o4)]

    [_ (raise-argument-error 'parse-ipv4 "4 octets separated by dots" ip)]))

(define (make-ipv4 o1 o2 o3 o4)
  (ipv4-address (+ o4
                   (* o3 256)
                   (* o2 (expt 256 2))
                   (* o1 (expt 256 3)))))

(define (ipv4-octet? n)
  (and (>= n 0)
       (<= n 255)))
