#lang racket/base

(require net/ip
         net/ip/ipv4
         rackunit)

(define ipv4-tests
  (test-suite
   "ipv4"

   (test-suite
    "parse-ipv4"

    (test-case "parses valid addresses"
      (check-equal? (parse-ipv4 "0.0.0.0") (ipv4-address 0))
      (check-equal? (parse-ipv4 "0.0.0.1") (ipv4-address 1))
      (check-equal? (parse-ipv4 "1.0.0.1") (ipv4-address 16777217))
      (check-equal? (parse-ipv4 "1.1.1.1") (ipv4-address 16843009))
      (check-equal? (parse-ipv4 "255.255.255.255") (ipv4-address 4294967295)))

    (test-case "parses shorthand addresses"
      (check-equal? (parse-ipv4 "127") (ipv4-address 127))
      (check-equal? (parse-ipv4 "127.1") (ipv4-address 2130706433))
      (check-equal? (parse-ipv4 "127.255.1") (ipv4-address 2147418113))
      (check-equal? (parse-ipv4 "127.1") (parse-ipv4 "127.0.0.1"))
      (check-equal? (parse-ipv4 "127.255.1") (parse-ipv4 "127.255.0.1")))

    (test-case "raises contract errors if given invalid addresses"
      (check-exn exn:fail:contract? (lambda () (parse-ipv4 "")))
      (check-exn exn:fail:contract? (lambda () (parse-ipv4 "256.256.256.256")))
      (check-exn exn:fail:contract? (lambda () (parse-ipv4 "255.255.255.255.255")))))

   (test-suite
    "ip-address-dec"

    (test-case "decrements ip addresses"
      (check-equal? (ip-address-dec (parse-ipv4 "0.0.0.1")) (parse-ipv4 "0.0.0.0"))
      (check-equal? (ip-address-dec (parse-ipv4 "0.0.1.0") 256) (parse-ipv4 "0.0.0.0")))

    (test-case "raises contract errors if decrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-dec (parse-ipv4 "0.0.0.0"))))))

   (test-suite
    "ip-address-inc"

    (test-case "increments ip addresses"
      (check-equal? (ip-address-inc (parse-ipv4 "0.0.0.0")) (parse-ipv4 "0.0.0.1"))
      (check-equal? (ip-address-inc (parse-ipv4 "0.0.0.0") 256) (parse-ipv4 "0.0.1.0")))

    (test-case "raises contract errors if incrementing beyond the valid IPv4 range"
      (check-exn exn:fail:contract? (lambda () (ip-address-inc (parse-ipv4 "255.255.255.255"))))))

   (test-suite
    "ip-address->string"

    (test-case "converts ip addresses to strings"
      (check-equal? (ip-address->string (parse-ipv4 "127")) "0.0.0.127")
      (check-equal? (ip-address->string (parse-ipv4 "127.0.0.0")) "127.0.0.0")
      (check-equal? (ip-address->string (parse-ipv4 "192.168.0.1")) "192.168.0.1")
      (check-equal? (ip-address->string (parse-ipv4 "255.255.255.255")) "255.255.255.255")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests ipv4-tests))
