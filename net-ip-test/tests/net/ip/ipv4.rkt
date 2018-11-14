#lang racket/base

(require net/ip
         net/ip/ipv4
         rackunit)

(define ipv4-tests
  (test-suite
   "ipv4"

   (test-suite
    "bytes->ipv4-address"

    (test-case "parses valid addresses"
      (check-equal? (bytes->ipv4-address #"\x00\x00\x00\x7F") (ipv4-address 127))
      (check-equal? (bytes->ipv4-address #"\x7F\x00\x00\x01") (string->ipv4-address "127.0.0.1"))
      (check-equal? (bytes->ipv4-address #"\xFF\xFF\xFF\xFF") (string->ipv4-address "255.255.255.255"))))

   (test-suite
    "string->ipv4-address"

    (test-case "parses valid addresses"
      (check-equal? (string->ipv4-address "0.0.0.0") (ipv4-address 0))
      (check-equal? (string->ipv4-address "0.0.0.1") (ipv4-address 1))
      (check-equal? (string->ipv4-address "1.0.0.1") (ipv4-address 16777217))
      (check-equal? (string->ipv4-address "1.1.1.1") (ipv4-address 16843009))
      (check-equal? (string->ipv4-address "255.255.255.255") (ipv4-address 4294967295)))

    (test-case "parses shorthand addresses"
      (check-equal? (string->ipv4-address "127") (ipv4-address 127))
      (check-equal? (string->ipv4-address "127.1") (ipv4-address 2130706433))
      (check-equal? (string->ipv4-address "127.255.1") (ipv4-address 2147418113))
      (check-equal? (string->ipv4-address "127.1") (string->ipv4-address "127.0.0.1"))
      (check-equal? (string->ipv4-address "127.255.1") (string->ipv4-address "127.255.0.1")))

    (test-case "raises contract errors if given invalid addresses"
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "")))
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "256.256.256.256")))
      (check-exn exn:fail:contract? (lambda () (string->ipv4-address "255.255.255.255.255")))))

   (test-suite
    "ip-address-dec"

    (test-case "decrements ip addresses"
      (check-equal? (ip-address-dec (string->ipv4-address "0.0.0.1")) (string->ipv4-address "0.0.0.0"))
      (check-equal? (ip-address-dec (string->ipv4-address "0.0.1.0") 256) (string->ipv4-address "0.0.0.0")))

    (test-case "raises contract errors if decrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-dec (string->ipv4-address "0.0.0.0"))))))

   (test-suite
    "ip-address-inc"

    (test-case "increments ip addresses"
      (check-equal? (ip-address-inc (string->ipv4-address "0.0.0.0")) (string->ipv4-address "0.0.0.1"))
      (check-equal? (ip-address-inc (string->ipv4-address "0.0.0.0") 256) (string->ipv4-address "0.0.1.0")))

    (test-case "raises contract errors if incrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-inc (string->ipv4-address "255.255.255.255"))))))

   (test-suite
    "ip-address->bytes"

    (test-case "converts ip addresses to bytes in network (big endian) order"
      (check-equal? (ip-address->bytes (string->ipv4-address "127"))           #"\x00\x00\x00\x7F")
      (check-equal? (ip-address->bytes (string->ipv4-address "127.0.0.1"))     #"\x7F\x00\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv4-address "192.168.0.1"))   #"\xC0\xA8\x00\x01")
      (check-equal? (ip-address->bytes (string->ipv4-address "255.255.255.0")) #"\xFF\xFF\xFF\x00")))

   (test-suite
    "ip-address->string"

    (test-case "converts ip addresses to strings"
      (check-equal? (ip-address->string (string->ipv4-address "127")) "0.0.0.127")
      (check-equal? (ip-address->string (string->ipv4-address "127.0.0.0")) "127.0.0.0")
      (check-equal? (ip-address->string (string->ipv4-address "192.168.0.1")) "192.168.0.1")
      (check-equal? (ip-address->string (string->ipv4-address "255.255.255.255")) "255.255.255.255")))

   (test-suite
    "ip-address->version"

    (test-case "always returns 4"
      (check-eq? (ip-address->version (string->ipv4-address "127")) 4)
      (check-eq? (ip-address->version (string->ipv4-address "255.255.255.255")) 4)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests ipv4-tests))
