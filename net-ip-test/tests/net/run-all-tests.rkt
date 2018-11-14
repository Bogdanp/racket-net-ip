#lang racket/base

(require rackunit
         "ip.rkt"
         "ip/ipv4.rkt")

(define all-ip-tests
  (test-suite
   "net-ip-lib"

   ip-tests
   ipv4-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-ip-tests))
