#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("libp2p/multiaddr"
    "libp2p/peer"
    "libp2p/daemon"
    "libp2p/client"
    "libp2p/dht"
    "libp2p/pubsub"
    "libp2p/connmgr"
    "libp2p/cid"
    "libp2p/pb/p2pd"
    "libp2p/pb/identify"
    "libp2p"
    (exe: "example/libp2p-echo")
    (exe: "example/libp2p-chat")
    (exe: "example/dht-crawl")
    ))
