;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p package interface

(import :vyzo/libp2p/daemon
        :vyzo/libp2p/client
        :vyzo/libp2p/peer
        :vyzo/libp2p/multiaddr)
(export
  ;; :vyzo/libp2p/daemon
  current-libp2p-daemon
  start-libp2p-daemon!
  stop-libp2p-daemon!
  use-libp2-daemon!
  ;; :vyzo/libp2p/client
  libp2p-error?
  client?
  (struct-out stream)
  stream-close
  stream-write-delimited
  stream-read-delimited
  open-libp2p-client
  libp2p-identify
  libp2p-connect
  libp2p-stream
  libp2p-listen
  libp2p-close
  ;; :vyzo/libp2p/peer
  (struct-out ID peer-info)
  ID->string string->ID
  peer-info->string peer-info->string* string->peer-info
  ;; :vyzo/libp2p/multiaddr
  (struct-out multiaddr)
  multiaddr->string
  string->multiaddr
  defprotocol)
