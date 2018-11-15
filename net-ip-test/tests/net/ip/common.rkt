#lang racket/base

(require net/ip/common
         rackunit)

(provide common-tests)

(define common-tests
  (test-suite
   "common"

   (test-suite
    "bitcount"

    (test-case "counts the set high bits in a number"
      (check-eq? (bitcount 5) 1)
      (check-eq? (bitcount 5 32) 0)
      (check-eq? (bitcount 255) 8)
      (check-eq? (bitcount 255 16) 0)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests common-tests))
