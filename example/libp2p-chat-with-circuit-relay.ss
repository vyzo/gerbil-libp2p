#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; © vyzo
;;; Modified by: @bennewhall for circuit relay
;;; libp2p chat example that uses circuit relay
;;;
;;; ------------------------------
;;; Example Usage in local network
;;; ------------------------------
;;; Open 3 terminal windows:
;;;
;;; In the first terminal, start your circuit relay node, which allows for connections
;;; between chat nodes
;;; who may be unable to directly connect to one another.
;;;
;;; ```
;;; > ./libp2p-chat-with-circuit-relay.ss gen-circuit-relay /ip4/0.0.0.0/tcp/10333
;;; I am /ip4/127.0.0.1/tcp/10333/p2p/<CircuitRelayPeerID>
;;; I am /ip4/192.168.0.7/tcp/10333/p2p/<CircuitRelayPeerID>
;;; ```
;;;
;;; In the second terminal, start a node (A) which listens for incoming connections
;;; and connects to the circuit relay node:
;;; ```
;;; >./libp2p-chat-with-circuit-relay.ss listen /ip4/0.0.0.0/tcp/10330/ \
;;;  /ip4/127.0.0.1/tcp/10333/p2p/<CircuitRelayPeerID>
;;;
;;; I am /ip4/10.0.0.6/tcp/10330/p2p/<PeerId (A)>
;;; I am /ip4/127.0.0.1/tcp/10330/p2p/<PeerId (A)>
;;; ```
;;;
;;; In the third terminal, start another node (B), which dials the listening node (A) through the circuit relay:
;;; ```
;;; > ./libp2p-chat-with-circuit-relay.ss dial \
;;; /ip4/127.0.0.1/tcp/10333/p2p/<CircuitRelayPeerID>/p2p-circuit/p2p/<PeerId (A)> \
;;; /ip4/0.0.0.0/tcp/10300
;;;
;;; I am /ip4/10.0.0.6/tcp/10300/p2p/<PeerId (B)>
;;; I am /ip4/127.0.0.1/tcp/10300/p2p/<PeerId (B)>
;;; Connecting to /ip4/127.0.0.1/tcp/10333/p2p/<PeerId (A)>
;;; ```
;;;
;;; And start chatting!
;;;
;;;
;;; For more info on how circuit relays work and their uses, look at
;;; https://docs.libp2p.io/concepts/circuit-relay/

(import :gerbil/gambit/threads
        :gerbil/gambit/ports
        :std/getopt
        :std/sugar
        :std/iter
        :std/logger
        :std/net/bio
        :vyzo/libp2p)
(export main)

(def chat-proto "/chat/1.0.0")

(def (register-chat-handler! c)
  (libp2p-listen c [chat-proto] chat-handler))

;; the chat protocol [see https://github.com/libp2p/go-libp2p-examples/tree/master/chat]
;; peers write newline delimited messages to one another
(def (chat-handler s)
  (displayln "*** Incoming connection from " (peer-info->string (cdr (stream-info s))))
  (do-chat s)
  (displayln "*** STREAM CLOSED"))

(def (do-chat s)
  (let (reader (spawn chat-reader s))
    (chat-writer s)
    (thread-terminate! reader)
    (stream-close s)))

(def (chat-writer s)
  (let lp ()
    (display "> ")
    (let (line (read-line))
      (unless (eof-object? line)
        (bio-write-string line (stream-out s))
        (bio-write-char #\newline (stream-out s))
        (bio-force-output (stream-out s))
        (lp)))))

(def (chat-reader s)
  (let lp ()
    (let (line (bio-read-line (stream-in s)))
      (cond
       ((eof-object? line)
        (displayln "*** STREAM CLOSED"))
       ((string-empty? line)
        (lp))
       (else
        (write-u8 #x1b)
        (display "[32m")
        (display line)
        (write-u8 #x1b)
        (displayln "[0m")
        (display "> ")
        (lp))))))

;; executable program
(def (main . args)
  (def listen-cmd
    (command 'listen help: "listen for incoming connections"
             (argument 'host-addresses help: "Comma separated multi addresses")
             (argument 'circuit-relay value: string->peer-info help: "Multi address of the circuit relay")))
  (def dial-cmd
    (command 'dial help: "dial an echo peer"
             (argument 'peer value: string->peer-info help: "Address of peer to dial")
             (argument 'host-addresses  help: "Comma separated multi addresses")))
  (def gen-circuit-relay-cmd
    (command 'gen-circuit-relay help: "generate a circuit relay to connect to"
             (argument 'host-addresses help: "Comma seperated multiaddresses to host at")))
  (def help-cmd
    (command 'help help: "display help; help <command> for command help"
             (optional-argument 'command value: string->symbol)))
  (def gopt
    (getopt listen-cmd dial-cmd gen-circuit-relay-cmd))

  (start-logger!)
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (let-hash opt
       (case cmd
         ((listen)
          (do-listen .host-addresses .circuit-relay))
         ((dial)
          (do-dial .peer .host-addresses))
         ((gen-circuit-relay)
          (do-circuit-relay .host-addresses))
         ((help)
          (getopt-display-help-topic gopt .?command "libp2p-chat")))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "libp2p-chat" (current-error-port))
     (exit 1))))

(def (do-listen host-addresses circuit-relay)
  (let* ((c (open-libp2p-client host-addresses: host-addresses wait: 20))
         (self (libp2p-identify c)))
    (for (p (peer-info->string* self))
      (displayln "I am " p))
    (displayln "Listening for incoming connections")
    (libp2p-listen c [chat-proto] chat-handler)
    (displayln "Connecting to Circuit Relay")
    (libp2p-connect c circuit-relay) ;;This allows for the dialing peer to dial the listener through the circuit relay
    (displayln "Connected to Circuit Relay")
    (thread-sleep! +inf.0)))

(def (do-dial peer host-addresses)
  (displayln "dialing")
  (let* ((c (open-libp2p-client host-addresses: host-addresses wait: 20))
         (self (libp2p-identify c)))
    (for (p (peer-info->string* self))
      (displayln "I am " p))
    (displayln "Connecting to " (peer-info->string peer))
    (libp2p-connect c peer)
    (let (s (libp2p-stream c peer [chat-proto]))
      (do-chat s))))

;;start a libp2p daemon that allows for circuit relays
(def (do-circuit-relay host-addresses)
  (let* ((d (start-the-libp2p-daemon! host-addresses: host-addresses options: ["-relayHop"] wait: 20))
         (c (open-libp2p-client host-addresses: host-addresses wait: 20)))
    (for (p (peer-info->string* (libp2p-identify c)))
      (displayln "I am " p))
    (thread-sleep! +inf.0)))
