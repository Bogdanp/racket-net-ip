#lang scribble/manual

@(require (for-label net/ip
                     racket)
          scribble/example)

@title{IP Addresses}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@section{Introduction}

This package provides utilities for working with IP addresses and
networks in Racket.


@section{Generic IP Addresses and Networks}
@defmodule[net/ip]

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
  ]
}

@subsection{IP Address Operations}

@deftogether[(@defidform[#:kind "interface" gen:ip-address]
              @defproc[(ip-address? [addr any/c]) boolean?])]{
  The generic interface that specifies IP addresses.
}

@deftogether[(@defproc[(ip-address=? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address<? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address<=? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address>? [addr ip-address?] [other ip-address?]) boolean?]
              @defproc[(ip-address>=? [addr ip-address?] [other ip-address?]) boolean?])]{
  Compare @racket[addr] and @racket[other].
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

@deftogether[(@defidform[#:kind "interface" gen:network]
              @defproc[(network? [net any/c]) boolean?])]{
  The generic interface that specifies networks of IP addresses.
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
@defmodule[net/ip/ipv4]

This module exposes functions specific to working with IPv4 addresses
and networks.

@subsection{IPv4 Addresses}

@defproc[(ipv4-address? [addr any/c]) boolean?]{
  Return @racket[#t] when @racket[addr] is an IPv4 address.
}

@defproc[(ipv4-address [value exact-nonnegative-integer?]) ipv4-address?]{
  Return the IPv4 address represented by @racket[value].
  @racket[value] must be between 0 and 2^32-1, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(bytes->ipv4-address [bs bytes?]) ipv4-address?]{
  Unpack a byte array into an IPv4 address.  Must be exactly four
  bytes long and in big endian order, otherwise an
  @racket[exn:fail:contract?] is raised.
}

@defproc[(string->ipv4-address [ip string?]) ipv4-address?]{
  Parse an IPv4 address.  Includes support for compressed notation,
  similar to what is available on most UNIX systems.

  @examples[
    (require net/ip/ipv4)
    (string->ipv4-address "127.0.0.1")
    (string->ipv4-address "127.1")
    (string->ipv4-address "192.168.1")
  ]
}

@subsection{IPv4 Networks}

@defproc[(ipv4-network? [net any/c]) boolean?]{
  Return @racket[#t] when @racket[net] is an IPv4 network.
}

@defproc[(ipv4-network [addr ip-address?] [prefix exact-nonnegative-integer?]) ipv4-network?]{
  Return the IPv4 network represented by @racket[addr] and
  @racket[prefix].  @racket[prefix] must be between 0 and 32,
  otherwise an @racket[exn:fail:contract?] is raised.
}

@defproc[(string->ipv4-network [ip string?]) ipv4-network?]{
  Parse CIDR block notation to an IPv4 network.  Raises an
  @racket[exn:fail:contract?] if the network address overlaps with a
  host address within the network.

  @examples[
    (require net/ip/ipv4)
    (string->ipv4-network "0.0.0.0/32")
    (string->ipv4-network "127.0/24")
    (string->ipv4-network "192.168.1.0/30")
  ]
}


@section{IPv6}

@subsection{IPv6 Addresses}

IPv6 addresses are not yet supported.

@subsection{IPv6 Networks}

IPv6 networks are not yet supported.
