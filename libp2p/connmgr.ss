;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p Connection Manager API

(import :vyzo/libp2p/peer
        :vyzo/libp2p/client
        :vyzo/libp2p/pb/p2pd)
(export #t)

(def (libp2p-tag-peer c p tag w)
  (let (req (Request
             type: 'CONNMANAGER
             connManager:
             (ConnManagerRequest
              type: 'TAG_PEER
              peer: (ID-bytes p)
              tag: tag
              weight: w)))
    (control-request c req void)))

(def (libp2p-untag-peer c p tag)
  (let (req (Request
             type: 'CONNMANAGER
             connManager:
             (ConnManagerRequest
              type: 'UNTAG_PEER
              peer: (ID-bytes p)
              tag: tag)))
    (control-request c req void)))

(def (libp2p-trim-open-connections c)
  (let (req (Request
             type: 'CONNMANAGER
             connManager:
             (ConnManagerRequest
              type: 'TRIM)))
    (control-request c req void)))
