#lang scribble/manual

@(require (for-label net/ip
                     racket)
          scribble/example)

@title{IP Addresses}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/ip]

@section{Introduction}

This package provides utilities for working with IP addresses and
networks in Racket.


@section{Generic IP Addresses and Networks}

All IP addresses and network versions support the operations that follow.

@deftogether[(@defproc[(make-ip-address [ip string?]) ip-address?]
              @defproc[#:link-target? #f (make-ip-address [ip bytes?]) ip-address?]
              @defproc[#:link-target? #f (make-ip-address [ip exact-nonnegative-integer?] [version (or/c 4 16)]) ip-address?])]{
  Parse an IP address.

  @examples[
    (require net/ip)
    (make-ip-address "127.0.0.1")
    (make-ip-address #"\x7F\x00\x00\x01")
    (make-ip-address 127 4)
    (make-ip-address "::1")
    (make-ip-address #"\xFF\xFF\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
    (make-ip-address "2001:db8::1")
  ]
}

@deftogether[(@defproc[(make-network [ip string?] [prefix exact-nonnegative-integer?]) network?]
              @defproc[#:link-target? #f (make-network [ip ip-address?] [prefix exact-nonnegative-integer?]) network?]
              @defproc[#:link-target? #f (make-network [cidr string?]) network?])]{
  Parse a network.

  @examples[
    (require net/ip)
    (make-network "127.0.0.0/24")
    (make-network "127.0.0.0" 24)
    (make-network (make-ip-address "127.0.0.0") 24)
    (make-network "::1/128")
    (make-network "::1" 128)
    (make-network (make-ip-address "::1") 128)
  ]
}

@subsection{IP Address Operations}

@defproc[(ip-address? [addr any/c]) boolean?]{
  Return @racket[#t] when @racket[addr] is an IP address.
}

@deftogether[(@defproc[(ip-address=? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address<? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address<=? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address>? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address>=? [addr ip-address?] [other ip-address?]) boolean?])]{
  Compare @racket[addr] and @racket[other].  These functions raise
  @racket[exn:fail:contract?] when the two addresses are of different
  versions.
}

@defproc[(ip-address-dec [addr ip-address?] [n exact-integer? 1]) ip-address?]{
  Return an @racket[ip-address?] smaller than @racket[addr] by @racket[n].
  Raises @racket[exn:fail:contract?] if the resulting address would be
  smaller than @racket[0].
}

@defproc[(ip-address-inc [addr ip-address?] [n exact-integer? 1]) ip-address?]{
  Return an @racket[ip-address?] greater than @racket[addr] by @racket[n].
  Raises @racket[exn:fail:contract?] if the resulting address would be
  greater than the maximum address for that particular version.
}

@defproc[(ip-address-size [addr ip-address?]) (or/c 32 128)]{
  Return @racket[32] or @racket[128] depending on the version of
  @racket[addr].  This number represents the size in bits of each type
  of address.
}

@defproc[(ip-address-version [addr ip-address?]) (or/c 4 6)]{
  Return @racket[4] or @racket[6] depending on the version of @racket[addr].
}

@defproc[(ip-address->bytes [addr ip-address?]) bytes?]{
  Pack @racket[addr] into an array of bytes in network order (big endian).
}

@defproc[(ip-address->number [addr ip-address?]) exact-nonnegative-integer?]{
  Convert @racket[addr] to a number.
}

@defproc[(ip-address->string [addr ip-address?]) string?]{
  Convert @racket[addr] to a string.
}


@subsection{Network Operations}

@defproc[(network? [net any/c]) boolean?]{
  Return @racket[#t] when @racket[net] is a network.
}

@defproc[(network [addr ip-address?] [prefix exact-nonnegative-integer?]) network?]{
  Construct a network from @racket[addr] and @racket[prefix].  Raises
  @racket[exn:fail:contract?] if @racket[prefix] is too large for
  @racket[addr].
}

@defproc[(network-address [net network?]) ip-address?]{
  Return the network address for @racket[net].
}

@defproc[(network-prefix [net network?]) exact-nonnegative-integer?]{
  Return the prefix for @racket[net].
}

@defproc[(network-hostmask [net network?]) ip-address?]{
  Return the host mask for @racket[net].
}

@defproc[(network-netmask [net network?]) ip-address?]{
  Return the net mask for @racket[net].
}

@defproc[(network-size [net network?]) exact-nonnegative-integer?]{
  Return the total number of ip addresses in @racket[net].
}

@defproc[(network-member [net network?] [addr ip-address?]) boolean?]{
  Return @racket[#t] if @racket[addr] is a member of @racket[net].
}

@defproc[(network-version [net network?]) (or/c 4 6)]{
  Return @racket[4] or @racket[6] depending on the version of @racket[net].
}

@defproc[(network->string [net network?]) string?]{
  Convert @racket[net] to a string.
}

@defproc[(network->stream [net network?]) (stream/c ip-address?)]{
  Convert @racket[net] to a stream of all the ip addresses contained
  inside it.
}


@section{IPv4}

These functions are specific to IPv4 addresses.

@subsection{IPv4 Addresses}

@defproc[(ipv4-address? [addr any/c]) boolean?]{
  Return @racket[#t] when @racket[addr] is an IPv4 address.
}

@defproc[(bytes->ipv4-address [bs bytes?]) ipv4-address?]{
  Unpack a byte array into an IPv4 address.  Must be exactly four
  bytes long and in big endian order, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(number->ipv4-address [value exact-nonnegative-integer?]) ipv4-address?]{
  Return the IPv4 address represented by @racket[value].
  @racket[value] must be between 0 and 2^32-1, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(string->ipv4-address [ip string?]) ipv4-address?]{
  Parse an IPv4 address.  Includes support for compressed notation,
  similar to what is available on most UNIX systems.

  @examples[
    (require net/ip)
    (string->ipv4-address "127.0.0.1")
    (string->ipv4-address "127.1")
    (string->ipv4-address "192.168.1")
  ]
}


@section{IPv6}

These functions are specific to IPv6 addresses.

@subsection{IPv6 Addresses}

@defproc[(ipv6-address? [addr any/c]) boolean?]{
  Return @racket[#t] when @racket[addr] is an IPv6 address.
}

@defproc[(bytes->ipv6-address [ip bytes?]) ipv6-address?]{
  Unpack a byte array into an IPv6 address.  Must be exactly four
  bytes long and in big endian order, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(number->ipv6-address [value exact-nonnegative-integer?]) ipv6-address?]{
  Return the IPv6 address represented by @racket[value].
  @racket[value] must be between 0 and 2^128-1, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(string->ipv6-address [ip string?]) ipv6-address?]{
  Parse an IPv6 address.

  @examples[
    (require net/ip)
    (string->ipv6-address "::1")
    (string->ipv6-address "ff:ab:cd::ff")
    (string->ipv6-address "201:db:ee::1")
    (string->ipv6-address "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
  ]
}
