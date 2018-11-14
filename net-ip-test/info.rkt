#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "net-ip-lib"
                     "quickcheck"
                     "rackunit-lib"))

(define update-implies '("net-ip-lib"))
