#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("libp2p/pb/p2pd.proto"
    "libp2p/multiaddr"
    "libp2p/peer"
    "libp2p/daemon"
    "libp2p/client"
    ))
