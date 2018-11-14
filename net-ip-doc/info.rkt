#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("net-ip-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("net-ip-lib"))
