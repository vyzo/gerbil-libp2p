# Gerbil libp2p

Implements [libp2p](https://github.com/libp2p) bindings for Gerbil, using the
[libp2p daemon](https://github.com/libp2p/go-libp2p-daemon).

<!-- toc -->

- [Installation](#installation)
- [Examples](#examples)
- [Reference](#reference)
  * [Daemon Control](#daemon-control)
  * [libp2p API](#libp2p-api)
  * [Stream Objects](#stream-objects)
  * [Peers and Addresses](#peers-and-addresses)
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
                      [address: (address #f)])
 daemon  := string; the daemon executable
 options := list of strings; options to pass to the daemon
 address := daemon unix socket path
=> <daemon>
```

Ensures that a daemon is running, and starts it if there isn't one.

#### stop-libp2p-daemon!
```
(stop-libp2p-daemon! [daemon = (current-libp2p-daemon)])
```

Kills the libp2p daemon if it was started with `start-libp2p-daemon!`.

#### use-libp2-daemon!
```
(use-libp2-daemon! path)
  path := string; daemon unix socket path
=> <daemon>
```

Sets the current daemon to an external process with `path` as the unix control
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
(libp2p-connect c peer)
  c    := client
  peer := peer-info
```

Ensures there is a connection open to `peer`.

#### libp2p-stream
```
(libp2p-stream c peer protos)
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

## License

MIT; © 2018 vyzo
