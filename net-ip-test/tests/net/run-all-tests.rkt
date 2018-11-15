#lang racket/base

(require rackunit
         "ip/address.rkt"
         "ip/common.rkt"
         "ip/network.rkt")

(define all-ip-tests
  (test-suite
   "net/ip"

   common-tests
   ip-tests
   network-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-ip-tests))
