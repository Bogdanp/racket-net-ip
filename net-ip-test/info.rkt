#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "net-ip-lib"
                     "rackunit-lib"))

(define update-implies '("net-ip-lib"))
