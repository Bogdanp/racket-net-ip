#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)
(define deps '("base"))
(define build-deps '("net-ip-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("net-ip-lib"))
