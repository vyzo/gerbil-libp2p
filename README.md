# Gerbil libp2p

Implements [libp2p](https://github.com/libp2p) bindings for Gerbil, using the
[libp2p daemon](https://github.com/libp2p/go-libp2p-daemon).

<!-- toc -->

- [Installation](#installation)
- [Examples](#examples)
- [Reference](#reference)
  * [Daemon Control](#daemon-control)
  * [libp2p API](#libp2p-api)
  * [DHT API](#dht-api)
  * [PubSub API](#pubsub-api)
  * [Stream Objects](#stream-objects)
  * [Peers and Addresses](#peers-and-addresses)
  * [Content Identifiers](#content-identifiers)
- [License](#license)

<!-- tocstop -->

## Installation

Requires a very recent Gerbil with [protobuf support](https://github.com/vyzo/gerbil/pull/133).

```
gxpkg install github.com/vyzo/gerbil-libp2p
```

You need to have have the libp2p daemon executable (`p2pd`) in your
path or have a running daemon you can connect to.

## Examples

For example code, interoperable with its Go implementation:
- [echo](example/libp2p-echo.ss) implements the echo protocol corresponding to
  the [echo libp2p example](https://github.com/libp2p/go-libp2p-examples/tree/master/echo).
- [chat](example/libp2p-chat.ss) implements the chat protocol corresponding to
  the [chat libp2p example](https://github.com/libp2p/go-libp2p-examples/tree/master/chat).

Additional examples:
- [dht-crawl](example/dht-crawl.ss) is a little DHT crawler.

## Reference

Here we document the bindings provided by the `:vyzo/libp2p` library.

To use the library:
```
(import :vyzo/libp2p)
```

### Daemon Control

You don't normally have to start the daemon, as `open-libp2p-client` will start one
automatically. But you may want to control the `p2pd` executable path and options or
want to reuse a running daemon.

#### start-libp2p-daemon!
```
(start-libp2p-daemon! [daemon: (daemon "p2pd")]
                      [options: (options [])]
                      [address: (address #f)]
                      [wait: (wait .4)])
 daemon  := string; the daemon executable
 options := list of strings; options to pass to the daemon
 address := daemon unix socket path
 wait    := how long to wait for daemon to initialize
=> <daemon>
```

Starts a new daemon running without regard to `current-libp2p-daemon`.
If a daemon is already running at `current-libp2p-daemon`,
this ignores that and creates a new one anyway.

#### start-the-libp2p-daemon!
```
(start-the-libp2p-daemon! [daemon: (daemon "p2pd")]
                          [options: (options [])]
                          [address: (address #f)]
                          [wait: (wait .4)])
 daemon  := string; the daemon executable
 options := list of strings; options to pass to the daemon
 address := daemon unix socket path
 wait    := how long to wait for daemon to initialize
=> <daemon>
```

Ensures that a daemon is running at `current-libp2p-daemon`,
and starts and sets it only if there isn't one already.

#### stop-libp2p-daemon!
```
(stop-libp2p-daemon! [daemon = (current-libp2p-daemon)])
```

Kills the given libp2p daemon if it was started with `start-libp2p-daemon!`.
This does not change the value inside `current-libp2p-daemon`,
even if the `daemon` argument is not provided or is equal to that value.

#### stop-the-libp2p-daemon!
```
(stop-the-libp2p-daemon!)
```

Kills the libp2p daemon at `current-libp2p-daemon`,
and sets the `current-libp2p-daemon` to false.

#### use-libp2p-daemon!
```
(use-libp2p-daemon! path)
  path := string; daemon unix socket path
=> <daemon>
```

Sets the `current-libp2p-daemon` to an external process with `path` as the unix control
socket path.


#### current-libp2p-daemon
```
(current-libp2p-daemon [daemon])
```

Parameter containing the current libp2p daemon.




### libp2p API

#### open-libp2p-client
```
(open-libp2p-client [path = #f])
  path := string; unix socket path to use for incoming streams
=> <client>
```

Creates a client object; starts a libp2p daemon if necessary.

#### libp2p-client?
```
(libp2p-client? obj)
=> boolean
```

Returns true if the object is a libp2p client object.

#### libp2p-identify
```
(libp2p-identify c)
  c := client
=> peer-info
```

Identifies the daemon.

#### libp2p-connect
```
(libp2p-connect c peer [timeout: timeout = #f])
  c    := client
  peer := peer-info
```

Ensures there is a connection open to `peer`.

#### libp2p-disconnect
```
(libp2p-disconnect c peer)
  c    := client
  peer := ID
```

Closes any open connections to `peer`.

#### libp2p-stream
```
(libp2p-stream c peer protos [timeout: timeout = #f])
  c      := client
  peer   := ID or peer-info; the peer for the stream
  protos := list of strings; the protocols to use for the stream
=> stream
```

Opens a new stream to peer identified by `id`.

#### libp2p-listen
```
(libp2p-listen c protos handler)
  c       := client
  protos  := list of strings; the protocols to handle
  handler := lambda (stream); the protocol handler
```

Sets the stream handler for `protos` to `handler`.
On new incoming streams, the `handler` is dispatched in a new thread
with the stream as an argument.

#### libp2p-close
```
(libp2p-close c)
  c := client
```

Closes a libp2p client.

#### libp2p-error?
```
(libp2p-error? obj)
=> boolean
```

Returns true if the obj is an error raised in a control operation to the daemon.


#### libp2p-list-peers
```
(libp2p-list-peers c)
  c := client
=> list of peer-info
```

Lists the peers currently connected to the daemon.


### DHT API
#### dht-find-peer
```
(dht-find-peer c p [timeout: timeout = #f])
  c := client
  p := ID
=> peer-info
```

Finds a peer in the DHT.

#### dht-find-peers-connected-to-peer*
```
(dht-find-peers-connected-to-peer* c p [timeout: timeout = #f])
  c := client
  p := ID
=> channel of peer-info
```

Asynchronously finds peers connected to a peer in the DHT;
the results are returned through a channel.

#### dht-find-peers-connected-to-peer
```
(dht-find-peers-connected-to-peer c p [timeout: timeout = #f])
  c := client
  p := ID
=> list of peer-info
```

Synchronously finds peers connected to a peer in the DHT;
the results are collected in a list.

#### dht-get-closest-peers*
```
(dht-get-closest-peers* c key [timeout: timeout = #f])
  c   := client
  key := string
=> channel of peer IDs
```

Asynchronously finds peers closest to a key in the DHT;
the results are returned through a channel.

#### dht-get-closest-peers
```
(dht-get-closest-peers c key [timeout: timeout = #f])
  c   := client
  key := string
=> list of peer IDs
```

Synchronously finds peers closest to a key in the DHT;
the results are returned in a list.

#### dht-get-public-key
```
(dht-get-public-key c p [timeout: timeout = #f])
  c := client
  p := ID
=> u8vector
```

Finds a peer's public key in the DHT.

#### dht-get-value
```
(dht-get-value c key [timeout: timeout = #f])
  c   := client
  key := string
=> u8vector
```

Finds the (best) value assocaited with a key in the DHT.

#### dht-search-value*
```
(dht-search-value* c key [timeout: timeout = #f])
  c   := client
  key := string
=> channel of u8vector
```

Asynchronously searches for values associated with a key in the DHT;
the results are returned through a channel.

#### dht-search-value
```
(dht-search-value c key [timeout: timeout = #f])
  c   := client
  key := string
=> list of u8vector
```

Synchronously searches for values associated with a key in the DHT;
the results are returned in a list.


#### dht-put-value
```
(dht-put-value c key val [timeout: timeout = #f])
  c   := client
  key := string
  val := u8vector
```

Associates a value with a key in the DHT.

#### dht-find-providers*
```
(dht-find-providers* c cid [count: count = #f] [timeout: timeout = #f])
  c   := client
  cid := CID
=> channel of peer-info
```

Asynchronously searches for providers of a Content Identifier in the DHT;
the results are returned throuogh a channel.

#### dht-find-providers
```
(dht-find-providers  c cid [count: count = #f] [timeout: timeout = #f])
  c   := client
  cid := CID
=> list of peer-info
```

Synchronously searches for providers of a Content Identifier in the DHT;
the results are returned in a list.

#### dht-provide
```
(dht-provide c cid [timeout: timeout = #f])
 c   := client
 cid := CID
```

Registers as a content provider for a Content Identifier in the DHT.

### PubSub API

#### pubsub-get-topics
```
(pubsub-get-topics c)
  c := client
=> list of string
```

Returns the pubsub topics to which the peer is subscribed

#### pubsub-list-peers
```
(pubsub-list-peers c topic)
  c     := client
  topic := string
=> list of ID
```

Returns the list of peers in a pubsub topic.

#### pubsub-publish
```
(pubsub-publish c topic data)
 c     := client
 topic := string
 data  := u8vector; the message to publish
```

Publishes a message to a pubsub topic.

#### pubsub-publisher
```
(pubsub-publisher c topic)
  c      := client
  topic: = string
=> (values publish cancel)
  publish := lambda (u8vector); function to publish the next message
  cancel  := lambda (); thunk to cancel the publisher
```

Creates a publisher function that reuses the connection to the daemon.

#### pubsub-subscribe
```
(pubsub-subscribe c topic)
  c      := client
  topic: = string
=> (values sub cancel)
  sub    := channel; message channel
  cancel := lambda (); thunk to cancel the subscription
  message := (vector ID data seqno topics signature key)
```

Subscribes to a topic; messages are delivered through the returned channel.

### Connection Manager API

#### libp2p-tag-peer
```
(libp2p-tag-peer c p tag w)
  c := client
  p := peer ID
  tag := string; tag to associate with the peer
  w := integer; weight to associate with the peer
```

Tags a peer in the connection manager.

#### libp2p-untag-peer
```
(libp2p-untag-peer c p tag)
  c := client
  p := peer ID
  tag := string; tag to remove from the peer
```

Untags a peer in the connection manager.

#### libp2p-trim-open-connections
```
(libp2p-trim-open-connections c)
  c := client
```

Asks the connection manager to trim open connections if necessary.

### Stream Objects
#### stream?
```
(stream? obj)
=> boolean
```

Returns true if the object is a libp2p stream object.

#### stream-in
```
(stream-in s)
  s := stream
=> input-buffer
```

Returns the stream input, as a [bio input buffer](https://cons.io/reference/net.html#binary-i-o-buffers).

#### stream-out
```
(stream-out s)
  s := stream
=> output-buffer
```

Returns the stream output, as a [bio output buffer](https://cons.io/reference/net.html#binary-i-o-buffers).

#### stream-info
```
(stream-info s)
  s := stream
=> [proto . peer-info]
```

Returns stream info, as a pair of protocol and `peer-info`.

#### stream-close
```
(stream-close s)
  s := stream
```

Closes the stream.

#### stream-close-output
```
(stream-close-output s)
  s := stream
```

Closes the output direction of stream.

#### stream-write-delimited
```
(stream-write-delimited s obj bio-write-e)
  s   := stream
  obj := any; the object to write
  bio-write-e := procedure; object marshaller
```

Writes a varint length delimited object, using `bio-write-e` as the serializer.
This is typically a protobuf object and its associated serializer.

#### stream-read-delimited
```
(stream-read-delimited s bio-read-e)
  s := stream
  bio-read-e := procedure
=> any
```

Reads a varint length delimited object, using `bio-read-e` as the deserializer.
This is typically a protobuf deserializer.


### Peers and Addresses
#### ID
```
(struct ID (bytes))
```

Peer IDs.

#### ID-&gt;string
```
(ID->string id)
  id := ID
=> string
```

Converts an ID to its base58 encoded string representation.

#### string-&gt;ID
```
(string->ID str)
  str := string
=> ID
```

Converts a base58 encoded string to a peer id.

#### peer-info
```
(struct peer-info (id addrs))
  id   := ID
  addr := list of multiaddr
```

Peer info objects, as a tuple of ID and addresses.

#### peer-info-&gt;string
```
(peer-info->string peer)
  peer := peer-info
=> string
```

Converts a `peer-info` to a string, using the first address if any.

#### peer-info-&gt;string*
```
(peer-info->string* peer)
  peer := peer-info
=> list of strings
```

Converts a `peer-info` to a list of strings, one for each address.

#### string-&gt;peer-info
```
(string->peer-info str)
  str := string
=> peer-info
```

Parses a string to a `peer-info`.

#### multiaddr
```
(struct multiaddr (bytes))
```

Peer address as a [multiaddr](https://github.com/multiformats/multiaddr).

#### multiaddr-&gt;string
```
(multiaddr->string ma)
  ma := multiaddr
=> string
```

Converts a multiaddr to a string.

#### string-&gt;multiaddr
```
(string->multiaddr str)
  str := string
=> multiaddr
```

Parses a string to a multiaddr.

### Content Identifiers

#### CID?
```
(CID? obj)
=> boolean
```

Returns true if the object is a Content Identifier

#### CIDv1
```
(CIDv1 c mh)
  c  := content code
  mh := multihash
=> CID
```

Constructs a CID v1 object.

#### CIDv1Raw
```
(CIDv1Raw mh)
  mh := multihash
=> CID
```

Costructs a Raw CID v1 object.

#### CIDv1Raw/sha256
```
(CIDv1Raw/sha256 content)
  content := string, u8vector, or input-port; the content to hash
=> CID
```

Constructs a Raw CID v1 object by hashing `content`.


#### CID-&gt;string
```
(CID->string cid)
  cid := CID
=> string
```

Converts a CID object to a string.

#### string-&gt;CID
```
(string->CID str)
  str := string
=> CID
```

Parses a string to a CID object.

#### CID-&gt;bytes
```
(CID->bytes cid)
  cid := CID
=> u8vector
```

Encodes a CID object to its binary representation.

#### bytes-&gt;CID
```
(bytes->CID bytes)
  bytes := u8vector
=> CID
```

Decodes a CID object from its binary representation.

#### multihash?
```
(multihash? obj)
=> boolean
```

Returns true if the object is a multihash object.

#### multihash-sha256
```
(multihash-sha256 content)
  content := string, u8vector, or input-port
=> multihash
```

Computes the sha256 multihash for `content`.


## License

MIT; © 2018 vyzo
