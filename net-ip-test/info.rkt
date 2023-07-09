#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "net-ip-lib"
                     ["rackcheck-lib" #:version "2.1"]
                     "rackunit-lib"))
(define update-implies '("net-ip-lib"))
