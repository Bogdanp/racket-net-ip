#lang racket/base

(require net/ip/address
         quickcheck
         quickcheck/generator
         racket/stream
         rackunit
         rackunit/quickcheck)

(provide ip-tests)

(define (choose-ipv4-address #:lo [lo 0]
                             #:hi [hi (sub1 (expt 2 32))])
  (lift->generator number->ipv4-address (choose-integer lo hi)))

(define (choose-ipv6-address #:lo [lo 0]
                             #:hi [hi (sub1 (expt 2 128))])
  (lift->generator number->ipv6-address (choose-integer lo hi)))

(define ip-tests
  (test-suite
   "ip"
   ipv4-tests
   ipv6-tests))

(define ipv4-tests
  (test-suite
   "ipv4"

   (test-suite
    "make-ip-address"

    (test-case "converts integers to ipv4"
      (check-equal? (make-ip-address 127 4) (number->ipv4-address 127)))

    (test-case "converts bytes to ipv4"
      (check-equal? (make-ip-address #"\x7F\xFF\xFF\x00") (string->ipv4-address "127.255.255.0")))

    (test-case "converts strings to ipv4"
      (check-equal? (make-ip-address "255.255.255.255") (string->ipv4-address "255.255.255.255")))

    (test-case "fails on other kinds of arguments"
      (check-exn exn:fail:contract? (lambda () (make-ip-address #"")))
      (check-exn exn:fail:contract? (lambda () (make-ip-address #"\x00\x00\x00\x00\x00")))))

   (test-suite
    "ipv4-address?"

    (test-case "#t when given an ipv4 addr"
      (check-true (ipv4-address? (make-ip-address "127.1"))))

    (test-case "#f when given an ipv6 addr"
      (check-false (ipv4-address? (make-ip-address "::1")))))

   (test-suite
    "ip-address=?"

    (test-case "#t when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv4-address)])
          (ip-address=? ip ip))))

    (test-case "#f when the addresses are different"
      (check-property
        (property ([ip (choose-ipv4-address #:lo 1)])
          (not (ip-address=? ip (ip-address-dec ip)))))))

   (test-suite
    "ip-address<?"

    (test-case "#f when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv4-address)])
          (not (ip-address<? ip ip)))))

    (test-case "#f when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:lo 1)])
          (not (ip-address<? ip (ip-address-dec ip))))))

    (test-case "#t when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:hi (- (expt 2 32) 2))])
          (ip-address<? ip (ip-address-inc ip)))))

    (test-case "fails when the addresses are different versions"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (ip-address<? (make-ip-address "127.1")
                                 (make-ip-address "::1"))))))

   (test-suite
    "ip-address<=?"

    (test-case "#t when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv4-address)])
          (ip-address<=? ip ip))))

    (test-case "#f when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:lo 1)])
          (not (ip-address<=? ip (ip-address-dec ip))))))

    (test-case "#t when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:hi (- (expt 2 32) 2))])
          (ip-address<=? ip (ip-address-inc ip))))))

   (test-suite
    "ip-address>?"

    (test-case "#f when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv4-address)])
          (not (ip-address>? ip ip)))))

    (test-case "#t when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:lo 1)])
          (ip-address>? ip (ip-address-dec ip)))))

    (test-case "#f when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:hi (- (expt 2 32) 2))])
          (not (ip-address>? ip (ip-address-inc ip)))))))

   (test-suite
    "ip-address>=?"

    (test-case "#t when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv4-address)])
          (ip-address>=? ip ip))))

    (test-case "#t when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:lo 1)])
          (ip-address>=? ip (ip-address-dec ip)))))

    (test-case "#f when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv4-address #:hi (- (expt 2 32) 2))])
          (not (ip-address>=? ip (ip-address-inc ip)))))))

   (test-suite
    "ip-address-dec"

    (test-case "decrements ip addresses"
      (check-property
        (property ([addr (choose-ipv4-address #:lo 1)])
          (= (ip-address->number (ip-address-dec addr))
             (sub1 (ip-address->number addr))))))

    (test-case "raises contract errors if decrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-dec (string->ipv4-address "0.0.0.0"))))))

   (test-suite
    "ip-address-inc"

    (test-case "increments ip addresses"
      (check-property
        (property ([addr (choose-ipv4-address #:hi (- (expt 2 32) 2))])
          (= (ip-address->number (ip-address-inc addr))
             (add1 (ip-address->number addr))))))

    (test-case "raises contract errors if incrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-inc (string->ipv4-address "255.255.255.255"))))))

   (test-suite
    "ip-address-size"

    (test-case "always returns 32"
      (check-property
        (property ([addr (choose-ipv4-address)])
          (= (ip-address-size addr) 32)))))

   (test-suite
    "ip-address-version"

    (test-case "always returns 4"
      (check-property
        (property ([addr (choose-ipv4-address)])
          (= (ip-address-version addr) 4)))))

   (test-suite
    "ip-address->bytes"

    (test-case "converts ip addresses to bytes in network (big endian) order"
      (check-equal? (ip-address->bytes (string->ipv4-address "127"))           #"\x00\x00\x00\x7F")
      (check-equal? (ip-address->bytes (string->ipv4-address "127.0.0.1"))     #"\x7F\x00\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv4-address "192.168.0.1"))   #"\xC0\xA8\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv4-address "255.255.255.0")) #"\xFF\xFF\xFF\x00")))

   (test-suite
    "bytes->ipv4-address"

    (test-case "parses valid addresses"
      (check-equal? (bytes->ipv4-address #"\x00\x00\x00\x7F") (number->ipv4-address 127))
      (check-equal? (bytes->ipv4-address #"\x7F\x00\x00\x01") (string->ipv4-address "127.0.0.1"))
      (check-equal? (bytes->ipv4-address #"\xFF\xFF\xFF\xFF") (string->ipv4-address "255.255.255.255")))

    (test-case "fails when given the wrong number of bytes"
      (check-exn exn:fail:contract? (lambda () (bytes->ipv4-address #"")))
      (check-exn exn:fail:contract? (lambda () (bytes->ipv4-address #"\xFF")))))

   (test-suite
    "ip-address->string"

    (test-case "converts ip addresses to strings"
      (check-equal? (ip-address->string (string->ipv4-address "127")) "0.0.0.127")
      (check-equal? (ip-address->string (string->ipv4-address "127.0.0.0")) "127.0.0.0")
      (check-equal? (ip-address->string (string->ipv4-address "192.168.0.1")) "192.168.0.1")
      (check-equal? (ip-address->string (string->ipv4-address "255.255.255.255")) "255.255.255.255")))

   (test-suite
    "string->ipv4-address"

    (test-case "parses valid addresses"
      (check-equal? (string->ipv4-address "0.0.0.0") (number->ipv4-address 0))
      (check-equal? (string->ipv4-address "0.0.0.1") (number->ipv4-address 1))
      (check-equal? (string->ipv4-address "1.0.0.1") (number->ipv4-address 16777217))
      (check-equal? (string->ipv4-address "1.1.1.1") (number->ipv4-address 16843009))
      (check-equal? (string->ipv4-address "255.255.255.255") (number->ipv4-address 4294967295)))

    (test-case "parses shorthand addresses"
      (check-equal? (string->ipv4-address "127") (number->ipv4-address 127))
      (check-equal? (string->ipv4-address "127.1") (number->ipv4-address 2130706433))
      (check-equal? (string->ipv4-address "127.255.1") (number->ipv4-address 2147418113))
      (check-equal? (string->ipv4-address "127.1") (string->ipv4-address "127.0.0.1"))
      (check-equal? (string->ipv4-address "127.255.1") (string->ipv4-address "127.255.0.1")))

    (test-case "raises contract errors if given invalid addresses"
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "")))
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "256.256.256.256")))
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "255.255.255.255.255")))))))

(define ipv6-tests
  (test-suite
   "ipv6"

   (test-suite
    "make-ip-address"

    (test-case "converts integers to ipv6"
      (check-equal? (make-ip-address 127 6) (number->ipv6-address 127)))

    (test-case "converts bytes to ipv6"
      (check-equal? (make-ip-address #"\xFF\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01")
                    (string->ipv6-address "FFFF:FF00::1")))

    (test-case "converts strings to ipv6"
      (check-equal? (make-ip-address "::") (string->ipv6-address "::"))
      (check-equal? (make-ip-address "::1") (string->ipv6-address "::1"))
      (check-equal? (make-ip-address "1:2::1") (string->ipv6-address "1:2:0:0:0:0:0:1"))))

   (test-suite
    "ipv6-address?"

    (test-case "#t when given an ipv6 addr"
      (check-true (ipv6-address? (make-ip-address "::1"))))

    (test-case "#f when given an ipv4 addr"
      (check-false (ipv6-address? (make-ip-address "127.1")))))

   (test-suite
    "ip-address<?"

    (test-case "#f when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv6-address)])
          (not (ip-address<? ip ip)))))

    (test-case "#f when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:lo 1)])
          (not (ip-address<? ip (ip-address-dec ip)))))

      (test-case "#t when the first addr is smaller than the second"
        (check-property
          (property ([ip (choose-ipv6-address #:hi (- (expt 2 32) 2))])
            (ip-address<? ip (ip-address-inc ip)))))))

   (test-suite
    "ip-address<=?"

    (test-case "#t when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv6-address)])
          (ip-address<=? ip ip))))

    (test-case "#f when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:lo 1)])
          (not (ip-address<=? ip (ip-address-dec ip))))))

    (test-case "#t when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:hi (- (expt 2 32) 2))])
          (ip-address<=? ip (ip-address-inc ip))))))

   (test-suite
    "ip-address>?"

    (test-case "#f when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv6-address)])
          (not (ip-address>? ip ip)))))

    (test-case "#t when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:lo 1)])
          (ip-address>? ip (ip-address-dec ip)))))

    (test-case "#f when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:hi (- (expt 2 32) 2))])
          (not (ip-address>? ip (ip-address-inc ip)))))))

   (test-suite
    "ip-address>=?"

    (test-case "#t when the addresses are the same"
      (check-property
        (property ([ip (choose-ipv6-address)])
          (ip-address>=? ip ip))))

    (test-case "#t when the first addr is larger than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:lo 1)])
          (ip-address>=? ip (ip-address-dec ip)))))

    (test-case "#f when the first addr is smaller than the second"
      (check-property
        (property ([ip (choose-ipv6-address #:hi (- (expt 2 32) 2))])
          (not (ip-address>=? ip (ip-address-inc ip)))))))

   (test-suite
    "ip-address-dec"

    (test-case "decrements ip addresses"
      (check-property
        (property ([addr (choose-ipv6-address #:lo 1)])
          (= (ip-address->number (ip-address-dec addr))
             (sub1 (ip-address->number addr))))))

    (test-case "raises contract errors if decrementing beyond the valid IPv6 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-dec (string->ipv6-address "0.0.0.0"))))))

   (test-suite
    "ip-address-inc"

    (test-case "increments ip addresses"
      (check-property
        (property ([addr (choose-ipv6-address #:hi (- (expt 2 32) 2))])
          (= (ip-address->number (ip-address-inc addr))
             (add1 (ip-address->number addr))))))

    (test-case "raises contract errors if incrementing beyond the valid IPv6 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-inc (string->ipv6-address "255.255.255.255"))))))

   (test-suite
    "ip-address-size"

    (test-case "always returns 128"
      (check-property
        (property ([addr (choose-ipv6-address)])
          (= (ip-address-size addr) 128)))))

   (test-suite
    "ip-address-version"

    (test-case "always returns 6"
      (check-property
        (property ([addr (choose-ipv6-address)])
          (= (ip-address-version addr) 6)))))

   (test-suite
    "ip-address->bytes"

    (test-case "converts ip addresses to bytes in network (big endian) order"
      (check-equal? (ip-address->bytes (string->ipv6-address "::"))         #"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
      (check-equal? (ip-address->bytes (string->ipv6-address "::1"))        #"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv6-address "FFFF::1"))    #"\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv6-address "FFFF:AB::1")) #"\xFF\xFF\x00\xAB\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01")))

   (test-suite
    "bytes->ipv6-address"

    (test-case "parses valid addresses"
      (check-equal? (bytes->ipv6-address (make-bytes 16 0))   (number->ipv6-address 0))
      (check-equal? (bytes->ipv6-address (make-bytes 16 255)) (string->ipv6-address "FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")))

    (test-case "fails when given the wrong number of bytes"
      (check-exn exn:fail:contract? (lambda () (bytes->ipv6-address #"")))
      (check-exn exn:fail:contract? (lambda () (bytes->ipv6-address #"\xFF")))))

   (test-suite
    "ip-address->string"

    (test-case "converts ip addresses to strings"
      (check-equal? (ip-address->string (string->ipv6-address "::")) "0:0:0:0:0:0:0:0")
      (check-equal? (ip-address->string (string->ipv6-address "::1")) "0:0:0:0:0:0:0:1")
      (check-equal? (ip-address->string (string->ipv6-address "FFF:AB::1")) "fff:ab:0:0:0:0:0:1")
      (check-equal? (ip-address->string (string->ipv6-address "FFFF::AB:CD:1")) "ffff:0:0:0:0:ab:cd:1")))

   (test-suite
    "string->ipv6-address"

    (test-case "parses valid addresses"
      (check-equal? (string->ipv6-address "::") (number->ipv6-address 0))
      (check-equal? (string->ipv6-address "::1") (number->ipv6-address 1)))

    (test-case "raises contract errors if given invalid addresses"
      (check-exn exn:fail:contract? (lambda () (string->ipv6-address "")))
      (check-exn exn:fail:contract? (lambda () (string->ipv6-address "::::")))
      (check-exn exn:fail:contract? (lambda () (string->ipv6-address "FF::BB::1")))
      (check-exn exn:fail:contract? (lambda () (string->ipv6-address "AA:BB:CC:DD:EE:FF::00:11")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests ip-tests))
