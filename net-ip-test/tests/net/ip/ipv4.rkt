#lang racket/base

(require net/ip
         net/ip/ipv4
         racket/stream
         rackunit)

(provide ipv4-tests)

(define ipv4-tests
  (test-suite
   "ipv4"

   (test-suite
    "ipv4-address"

    (test-suite
     "bytes->ipv4-address"

     (test-case "parses valid addresses"
       (check-equal? (bytes->ipv4-address #"\x00\x00\x00\x7F") (ipv4-address 127))
       (check-equal? (bytes->ipv4-address #"\x7F\x00\x00\x01") (string->ipv4-address "127.0.0.1"))
       (check-equal? (bytes->ipv4-address #"\xFF\xFF\xFF\xFF") (string->ipv4-address "255.255.255.255"))))

    (test-suite
     "string->ipv4-address"

     (test-case "parses valid addresses"
       (check-equal? (string->ipv4-address "0.0.0.0") (ipv4-address 0))
       (check-equal? (string->ipv4-address "0.0.0.1") (ipv4-address 1))
       (check-equal? (string->ipv4-address "1.0.0.1") (ipv4-address 16777217))
       (check-equal? (string->ipv4-address "1.1.1.1") (ipv4-address 16843009))
       (check-equal? (string->ipv4-address "255.255.255.255") (ipv4-address 4294967295)))

     (test-case "parses shorthand addresses"
       (check-equal? (string->ipv4-address "127") (ipv4-address 127))
       (check-equal? (string->ipv4-address "127.1") (ipv4-address 2130706433))
       (check-equal? (string->ipv4-address "127.255.1") (ipv4-address 2147418113))
       (check-equal? (string->ipv4-address "127.1") (string->ipv4-address "127.0.0.1"))
       (check-equal? (string->ipv4-address "127.255.1") (string->ipv4-address "127.255.0.1")))

     (test-case "raises contract errors if given invalid addresses"
       (check-exn exn:fail:contract? (lambda () (string->ipv4-address "")))
       (check-exn exn:fail:contract? (lambda () (string->ipv4-address "256.256.256.256")))
       (check-exn exn:fail:contract? (lambda () (string->ipv4-address "255.255.255.255.255")))))

    (test-suite
     "ip-address-dec"

     (test-case "decrements ip addresses"
       (check-equal? (ip-address-dec (string->ipv4-address "0.0.0.1")) (string->ipv4-address "0.0.0.0"))
       (check-equal? (ip-address-dec (string->ipv4-address "0.0.1.0") 256) (string->ipv4-address "0.0.0.0")))

     (test-case "raises contract errors if decrementing beyond the valid IPv4 range"
       (check-exn exn:fail:contract? (lambda () (ip-address-dec (string->ipv4-address "0.0.0.0"))))))

    (test-suite
     "ip-address-inc"

     (test-case "increments ip addresses"
       (check-equal? (ip-address-inc (string->ipv4-address "0.0.0.0")) (string->ipv4-address "0.0.0.1"))
       (check-equal? (ip-address-inc (string->ipv4-address "0.0.0.0") 256) (string->ipv4-address "0.0.1.0")))

     (test-case "raises contract errors if incrementing beyond the valid IPv4 range"
       (check-exn exn:fail:contract? (lambda () (ip-address-inc (string->ipv4-address "255.255.255.255"))))))

    (test-suite
     "ip-address-version"

     (test-case "always returns 4"
       (check-eq? (ip-address-version (string->ipv4-address "127")) 4)
       (check-eq? (ip-address-version (string->ipv4-address "255.255.255.255")) 4)))

    (test-suite
     "ip-address->bytes"

     (test-case "converts ip addresses to bytes in network (big endian) order"
       (check-equal? (ip-address->bytes (string->ipv4-address "127"))           #"\x00\x00\x00\x7F")
       (check-equal? (ip-address->bytes (string->ipv4-address "127.0.0.1"))     #"\x7F\x00\x00\x01")
       (check-equal? (ip-address->bytes (string->ipv4-address "192.168.0.1"))   #"\xC0\xA8\x00\x01")
       (check-equal? (ip-address->bytes (string->ipv4-address "255.255.255.0")) #"\xFF\xFF\xFF\x00")))

    (test-suite
     "ip-address->string"

     (test-case "converts ip addresses to strings"
       (check-equal? (ip-address->string (string->ipv4-address "127")) "0.0.0.127")
       (check-equal? (ip-address->string (string->ipv4-address "127.0.0.0")) "127.0.0.0")
       (check-equal? (ip-address->string (string->ipv4-address "192.168.0.1")) "192.168.0.1")
       (check-equal? (ip-address->string (string->ipv4-address "255.255.255.255")) "255.255.255.255"))))

   (test-suite
    "ipv4-network"

    (test-suite
     "string->ipv4-network"

     (test-case "parses valid CIDR blocks"
       (check-equal? (string->ipv4-network "192.168.1.0/24")
                     (ipv4-network (string->ipv4-address "192.168.1.0") 24)))

     (test-case "fails to parse invalid prefixes"
       (check-exn
        exn:fail:contract?
        (lambda ()
          (string->ipv4-network "127.0.0.1/102"))))

     (test-case "fails to parse invalid networks"
       (check-exn
        exn:fail:contract?
        (lambda ()
          (string->ipv4-network "192.168.1.0/22")))))

    (test-suite
     "network-address"

     (test-case "returns the network address"
       (check-equal? (network-address (string->ipv4-network "127.0.0.0/24"))
                     (string->ipv4-address "127.0.0.0"))))

    (test-suite
     "network-hostmask"

     (test-case "returns a network's host mask"
       (check-equal? (network-hostmask (string->ipv4-network "127.0.1.0/24"))
                     (string->ipv4-address "0.0.0.255"))
       (check-equal? (network-hostmask (string->ipv4-network "127.1.0.0/16"))
                     (string->ipv4-address "0.0.255.255"))))

    (test-suite
     "network-netmask"

     (test-case "returns a network's net mask"
       (check-equal? (network-netmask (string->ipv4-network "127.0.1.0/24"))
                     (string->ipv4-address "255.255.255.0"))
       (check-equal? (network-netmask (string->ipv4-network "127.1.0.0/16"))
                     (string->ipv4-address "255.255.0.0"))))

    (test-suite
     "network-size"

     (test-case "returns the size of the network"
       (check-eq? (network-size (string->ipv4-network "127.0.0.1/32")) (expt 2 0))
       (check-eq? (network-size (string->ipv4-network "127.0.0.0/24")) (expt 2 8))
       (check-eq? (network-size (string->ipv4-network "127.0.8.0/22")) (expt 2 10))
       (check-eq? (network-size (string->ipv4-network "0.0.0.0/0")) (expt 2 32))))

    (test-suite
     "network-member"

     (test-case "returns #f when an address isn't a member of a network"
       (check-false (network-member (string->ipv4-network "127.0.0.1/32")
                                    (string->ipv4-address "127.0.0.2")))

       (check-false (network-member (string->ipv4-network "127.0.0.0/24")
                                    (string->ipv4-address "127.0.1.0"))))

     (test-case "returns #t when an address is a member of a network"
       (check-true (network-member (string->ipv4-network "127.0.0.1/32")
                                   (string->ipv4-address "127.0.0.1")))

       (check-true (network-member (string->ipv4-network "127.0.0.0/24")
                                   (string->ipv4-address "127.0.0.1")))

       (check-true (network-member (string->ipv4-network "192.168.8.0/22")
                                   (string->ipv4-address "192.168.11.255")))

       (check-true (network-member (string->ipv4-network "0.0.0.0/0")
                                   (string->ipv4-address "127.0.0.1")))))

    (test-suite
     "network-version"

     (test-case "always returns 4"
       (check-eq? (network-version (string->ipv4-network "127.0.0.1/32")) 4)))

    (test-suite
     "network->string"

     (test-case "converts networks to strings"
       (check-equal? (network->string (string->ipv4-network "0.0.0.0/32")) "0.0.0.0/32")
       (check-equal? (network->string (string->ipv4-network "127.0.0.0/24")) "127.0.0.0/24")))

    (test-suite
     "network->stream"

     (test-case "lazily streams all the addresses in a network"
       (check-equal?
        (stream->list (network->stream (string->ipv4-network "127.0.0.1/32")))
        (list (string->ipv4-address "127.0.0.1")))

       (check-equal?
        (stream->list (network->stream (string->ipv4-network "127.0.0.0/30")))
        (list (string->ipv4-address "127.0.0.0")
              (string->ipv4-address "127.0.0.1")
              (string->ipv4-address "127.0.0.2")
              (string->ipv4-address "127.0.0.3")))

       (check-equal?
        (stream->list (network->stream (string->ipv4-network "192.168.100.96/28")))
        (list (string->ipv4-address "192.168.100.96")
              (string->ipv4-address "192.168.100.97")
              (string->ipv4-address "192.168.100.98")
              (string->ipv4-address "192.168.100.99")
              (string->ipv4-address "192.168.100.100")
              (string->ipv4-address "192.168.100.101")
              (string->ipv4-address "192.168.100.102")
              (string->ipv4-address "192.168.100.103")
              (string->ipv4-address "192.168.100.104")
              (string->ipv4-address "192.168.100.105")
              (string->ipv4-address "192.168.100.106")
              (string->ipv4-address "192.168.100.107")
              (string->ipv4-address "192.168.100.108")
              (string->ipv4-address "192.168.100.109")
              (string->ipv4-address "192.168.100.110")
              (string->ipv4-address "192.168.100.111"))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests ipv4-tests))
