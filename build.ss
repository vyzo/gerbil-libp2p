#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("libp2p/multiaddr"
    "libp2p/peer"
    "libp2p/daemon"
    "libp2p/client"
    "libp2p/pb/p2pd"
    "libp2p/pb/identify"
    "libp2p"
    (exe: "example/libp2p-echo")
    (exe: "example/libp2p-chat")
    ))
