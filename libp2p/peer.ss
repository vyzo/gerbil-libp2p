;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p peer related abstractions

(import :std/text/base58
        :vyzo/libp2p/multiaddr)
(export #t)

(defstruct ID (bytes)
  final: #t equal: #t)

(def (ID->string id)
  (base58-encode (ID-bytes id)))

(def (string->ID idstr)
  (ID (base58-decode idstr)))

(defstruct peer-info (id addrs)
  final: #t)
