;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p peer related abstractions

(import :std/text/base58
        :std/pregexp
        :vyzo/libp2p/multiaddr)
(export #t)

(defstruct ID (bytes)
  final: #t equal: #t)

(def (ID->string id)
  (base58-encode (ID-bytes id)))

(def (string->ID idstr)
  (ID (base58-decode idstr)))

(defstruct peer-info (id addrs)
  final: #t equal: #t)

(def (peer-info->string peer)
  (with ((peer-info id addrs) peer)
    (match addrs
    ([addr . _]
     (string-append (multiaddr->string addr) "/ipfs/" (ID->string id)))
    (else
     (string-append "/ipfs/" (ID->string id))))))

(def (peer-info->string* peer)
  (with ((peer-info id addrs) peer)
    (if (null? addrs)
      [(string-append "/ipfs/" (ID->string id))]
      (map (lambda (addr)
             (string-append (multiaddr->string addr) "/ipfs/" (ID->string id)))
           addrs))))

(def ipfs-part-rx
  (pregexp "/ipfs/"))

(def (string->peer-info str)
  (match (reverse (pregexp-split ipfs-part-rx str))
    ([id . rest]
     (peer-info (string->ID id)
                (let ((maddr (string-append-with-ipfs (reverse rest))))
                (if (string-empty? maddr)
                  []
                  [(string->multiaddr maddr)]))))
    (else
     (error "Malformed peer info" str))))

(def (string-append-with-ipfs lst)
  (if (equal? (cdr lst) '())
    (car lst)
    (apply string-append
      (list (car lst) "/ipfs/" (string-append-with-ipfs (cdr lst))))))
