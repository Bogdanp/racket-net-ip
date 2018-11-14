#lang racket/base

(require net/ip
         net/ip/ipv4
         rackunit)

(define ip-tests
  (test-suite
   "net/ip"

   (test-suite
    "make-ip-address"

    (test-case "converts integers to ipv4"
      (check-equal? (make-ip-address 127 4) (ipv4-address 127)))

    (test-case "converts bytes to ipv4"
      (check-equal? (make-ip-address #"\x7F\xFF\xFF\x00") (string->ipv4-address "127.255.255.0")))

    (test-case "converts strings to ipv4"
      (check-equal? (make-ip-address "255.255.255.255") (string->ipv4-address "255.255.255.255"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests ip-tests))
