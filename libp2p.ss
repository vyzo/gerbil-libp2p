;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p package interface

(import :vyzo/libp2p/daemon
        :vyzo/libp2p/client
        :vyzo/libp2p/dht
        :vyzo/libp2p/connmgr
        :vyzo/libp2p/cid
        :vyzo/libp2p/peer
        :vyzo/libp2p/multiaddr)
(export
  ;; :vyzo/libp2p/daemon
  current-libp2p-daemon
  start-libp2p-daemon!
  stop-libp2p-daemon!
  use-libp2p-daemon!
  ;; :vyzo/libp2p/client
  libp2p-error?
  (rename: client? libp2p-client?)
  stream?
  stream-in
  stream-out
  stream-info
  stream-close
  stream-write-delimited
  stream-read-delimited
  open-libp2p-client
  libp2p-identify
  libp2p-connect
  libp2p-stream
  libp2p-listen
  libp2p-close
  libp2p-list-peers
  ;; :vyzo/libp2p/dht
  dht-find-peer
  dht-find-peers-connected-to-peer*
  dht-find-peers-connected-to-peer
  dht-get-closest-peers*
  dht-get-closest-peers
  dht-get-public-key
  dht-get-value
  dht-search-value*
  dht-search-value
  dht-put-value
  dht-find-providers*
  dht-find-providers
  dht-provide
  ;; :vyzo/libp2p/connmgr
  libp2p-tag-peer
  libp2p-untag-peer
  libp2p-trim-open-connections
  ;; :vyzo/libp2p/cid
  CID?
  CIDv1 CIDv1Raw CIDv1Raw/sha256
  CID->string string->CID
  CID->bytes bytes->CID
  multihash? multihash-sha256
  ;; :vyzo/libp2p/peer
  (struct-out ID peer-info)
  ID->string string->ID
  peer-info->string peer-info->string* string->peer-info
  ;; :vyzo/libp2p/multiaddr
  (struct-out multiaddr)
  multiaddr->string
  string->multiaddr
  defprotocol)
