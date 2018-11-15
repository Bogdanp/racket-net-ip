#lang racket/base

(require net/ip/ip
         net/ip/network
         racket/port
         racket/stream
         rackunit)

(provide network-tests)

(define network-tests
  (test-suite
   "network"

   (test-suite
    "make-network"

    (test-case "converts ipv4 addresses with prefixes to ipv4 networks"
      (check-equal? (make-network (string->ipv4-address "127.0.0.0") 24)
                    (network (string->ipv4-address "127.0.0.0") 24)))

    (test-case "converts ipv6 addresses with prefixes to ipv6 networks"
      (check-equal? (make-network (string->ipv6-address "::") 0)
                    (network (string->ipv6-address "::") 0)))

    (test-case "converts strings with prefixes to ipv4 networks"
      (check-equal? (make-network "127.0.0.0" 24)
                    (network (string->ipv4-address "127.0.0.0") 24)))

    (test-case "converts strings with prefixes to ipv6 networks"
      (check-equal? (make-network "::" 0)
                    (network (string->ipv6-address "::") 0))
      (check-equal? (make-network "FFFF::" 60)
                    (network (string->ipv6-address "FFFF::") 60)))

    (test-case "converts strings to ipv4 networks"
      (check-equal? (make-network "127.0.0.0/24")
                    (network (string->ipv4-address "127.0.0.0") 24)))

    (test-case "converts strings to ipv6 networks"
      (check-equal? (make-network "FFFF::/60")
                    (network (string->ipv6-address "FFFF::") 60)))

    (test-case "fils when network has host bits set"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (make-network "192.168.1.1/24"))))

    (test-case "fails when given an invalid CIDR block"
      (check-exn exn:fail:contract? (lambda () (make-network "")))))

   (test-suite
    "network"

    (test-case "pretty-prints itself"
      (check-equal?
       (call-with-output-string
         (lambda (out)
           (print (make-network "192.168.1.0/24") out)))
       "(make-network \"192.168.1.0/24\")"))

    (test-case "fails when given an invalid prefix"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (network (make-ip-address "127.1") 128)))))

   (test-suite
    "network-address"

    (test-case "returns the network address"
      (check-equal? (network-address (make-network "127.0.0.0/24"))
                    (string->ipv4-address "127.0.0.0"))
      (check-equal? (network-address (make-network "FFFF::0/60"))
                    (string->ipv6-address "FFFF::0"))))

   (test-suite
    "network-hostmask"

    (test-case "returns a network's host mask"
      (check-equal? (network-hostmask (make-network "127.0.1.0/24"))
                    (string->ipv4-address "0.0.0.255"))
      (check-equal? (network-hostmask (make-network "127.1.0.0/16"))
                    (string->ipv4-address "0.0.255.255"))
      (check-equal? (network-hostmask (make-network "ABCD::/16"))
                    (string->ipv6-address "0:ffff:ffff:ffff:ffff:ffff:ffff:ffff"))))

   (test-suite
    "network-netmask"

    (test-case "returns a network's net mask"
      (check-equal? (network-netmask (make-network "127.0.1.0/24"))
                    (string->ipv4-address "255.255.255.0"))
      (check-equal? (network-netmask (make-network "127.1.0.0/16"))
                    (string->ipv4-address "255.255.0.0"))
      (check-equal? (network-netmask (make-network "ABCD::/16"))
                    (string->ipv6-address "FFFF::"))
      (check-equal? (network-netmask (make-network "ABCD::/24"))
                    (string->ipv6-address "FFFF:FF00::"))))

   (test-suite
    "network-size"

    (test-case "returns the size of the network"
      (check-eq? (network-size (make-network "127.0.0.1/32")) (expt 2 0))
      (check-eq? (network-size (make-network "127.0.0.0/24")) (expt 2 8))
      (check-eq? (network-size (make-network "127.0.8.0/22")) (expt 2 10))
      (check-eq? (network-size (make-network "0.0.0.0/0")) (expt 2 32))

      (check-eq? (network-size (make-network "::1/128")) (expt 2 0))
      (check-equal? (network-size (make-network "ab:cd:ef::/96")) (expt 2 32))
      (check-equal? (network-size (make-network "::/0")) (expt 2 128))))

   (test-suite
    "network-member"

    (test-case "returns #f when an address isn't a member of a network"
      (check-false (network-member (make-network "127.0.0.1/32")
                                   (string->ipv4-address "127.0.0.2")))

      (check-false (network-member (make-network "127.0.0.0/24")
                                   (string->ipv4-address "127.0.1.0")))

      (check-false (network-member (make-network "1234:abcd:ef00::1/128")
                                   (string->ipv6-address "1234:abcd:ef00::2")))

      (check-false (network-member (make-network "1234:abcd:ef00::/60")
                                   (string->ipv6-address "ffff:abcd:ef00::1")))

      (check-false (network-member (make-network "1234:abcd:ef00::/60")
                                   (string->ipv6-address "::1"))))

    (test-case "returns #t when an address is a member of a network"
      (check-true (network-member (make-network "127.0.0.1/32")
                                  (string->ipv4-address "127.0.0.1")))

      (check-true (network-member (make-network "127.0.0.0/24")
                                  (string->ipv4-address "127.0.0.1")))

      (check-true (network-member (make-network "192.168.8.0/22")
                                  (string->ipv4-address "192.168.11.255")))

      (check-true (network-member (make-network "0.0.0.0/0")
                                  (string->ipv4-address "127.0.0.1")))

      (check-true (network-member (make-network "::1/128")
                                  (string->ipv6-address "::1")))

      (check-true (network-member (make-network "1234:abcd:ef00::/60")
                                  (string->ipv6-address "1234:abcd:ef00::1")))

      (check-true (network-member (make-network "::/0")
                                  (string->ipv6-address "1234:abcd:ef00::1"))))

   (test-suite
    "network-version"

    (test-case "returns 4 for ipv4"
      (check-eq? (network-version (make-network "127.0.0.1/32")) 4))

    (test-case "returns 6 for ipv6"
      (check-eq? (network-version (make-network "::1/128")) 6)))

   (test-suite
    "network->string"

    (test-case "converts networks to strings"
      (check-equal? (network->string (make-network "0.0.0.0/32")) "0.0.0.0/32")
      (check-equal? (network->string (make-network "127.0.0.0/24")) "127.0.0.0/24")
      (check-equal? (network->string (make-network "::1/128")) "0:0:0:0:0:0:0:1/128")
      (check-equal? (network->string (make-network "ffff:ab::/60")) "ffff:ab:0:0:0:0:0:0/60")))

   (test-suite
    "network->stream"

    (test-case "lazily streams all the addresses in a network"
      (check-equal?
       (stream->list (network->stream (make-network "127.0.0.1/32")))
       (list (string->ipv4-address "127.0.0.1")))

      (check-equal?
       (stream->list (network->stream (make-network "::1/128")))
       (list (string->ipv6-address "::1")))

      (check-equal?
       (stream->list (network->stream (make-network "127.0.0.0/30")))
       (list (string->ipv4-address "127.0.0.0")
             (string->ipv4-address "127.0.0.1")
             (string->ipv4-address "127.0.0.2")
             (string->ipv4-address "127.0.0.3")))

      (check-equal?
       (stream->list (network->stream (make-network "ff:ab::/126")))
       (list (string->ipv6-address "ff:ab:0:0:0:0:0:0")
             (string->ipv6-address "ff:ab:0:0:0:0:0:1")
             (string->ipv6-address "ff:ab:0:0:0:0:0:2")
             (string->ipv6-address "ff:ab:0:0:0:0:0:3")))

      (check-equal?
       (stream->list (network->stream (make-network "192.168.100.96/28")))
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
  (run-tests network-tests))
