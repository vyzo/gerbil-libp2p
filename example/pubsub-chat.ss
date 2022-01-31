#!/usr/bin/env gxi
;;; ~~~~~Gerbil~~~~~
;;; @bennewhall
;;; A chat example that uses pubsub
;;; based off of libp2p-chat.ss by @vyzo
;;;
;;;~~~~~EXAMPLE USAGE~~~~~
;;;
;;;To begin, start a bootstrap node in a seperate terminal by running
;;;./pub-sub-chat.ss bootstrap /ip4/0.0.0.0/tcp/10333/
;;;
;;;After waiting a few seconds for the libp2p daemon to start up, you should get a list of multiaddresses to use to contact this node
;;;```
;;;starting up
;;;I am /ip4/127.0.0.1/tcp/10333/ipfs/<PeerID-A>
;;;I am /ip4/192.168.0.7/tcp/10333/ipfs/<PeerID-A>
;;;```
;;;
;;;Next, you need to start up other pubsub nodes to connect to this bootstrap node. In another terminal run
;;;
;;;./pub-sub-chat.ss connect <BootstrapNodeAddress> /ip4/0.0.0.0/tcp/10332/
;;;
;;;Where <BootstrapNodeAddress> is one of the addresses outputted when you started the Bootstrap node
;;;
;;;This should start up and connect to the bootstrap node. Type in the prompt to start chatting!
;;;
;;;If you would like, feel free to create more pubsub nodes to connect to this bootstrap node. In another terminal run
;;;./pub-sub-chat.ss connect <BootstrapNodeAddress> /ip4/0.0.0.0/tcp/<ADifferentPort>/
;;;
;;;Where <ADifferentPort> is a port unused by any other pubsub node
;;;
;;;
;;;To exit, Ctrl-C in each terminal window.
;;;


;; ~~~Import Statement~~~
(import :gerbil/gambit/threads
        :gerbil/gambit/ports
        :std/getopt
        :std/sugar
        :std/iter
        :std/logger
        :std/net/bio
        :vyzo/libp2p
        :std/text/base64
        :std/text/utf8
        :std/misc/channel)
(export main)


;;Main
;;After parsing the commandline arguments, this will call do-startup or display an error for incorrect arguments
(def (main . args)
  (def bootstrap-cmd
    (command 'bootstrap help: "listen for incoming connections"
               (argument 'host-addresses help: "Comma separated multi addresses")))
  (def connect-cmd
    (command 'connect help: "connect to a bootsrap"
             (argument 'peer value: string->peer-info help: "peer to initially connect to")
             (argument 'host-addresses  help: "Comma separated multi addresses")))
  (def help-cmd
    (command 'help help: "display help; help <command> for command help"
             (optional-argument 'command value: string->symbol)))
  (def gopt
    (getopt bootstrap-cmd connect-cmd help-cmd))

  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (let-hash opt
       (case cmd
         ((bootstrap) ;;In case where the command is to bootstrap
          (do-startup #f .host-addresses))
         ((connect) ;;where command is to connect
          (do-startup .peer .host-addresses))
         ((help) ;;where command is to help
          (getopt-display-help-topic gopt .?command "pub-sub-chat")))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "pub-sub-chat" (current-error-port))
     (exit 1))))


;; do-startup
;; Parameters:
;; peer: #f or a peer Addresss
;; host-addresses: a list of multiaddresses
;;
;;If peer is #f, do-startup will not try to connect to any peers and will subscribe to the topic "chat"
;;
;;If peer is anything else, do-startup will try to connect to peer and will subscribe to the topic "chat"
(def (do-startup peer host-addresses)
   (displayln "starting up")

   ;start the daemon with custom arguments -pubsub and -connManager to enable pubsub
   (start-libp2p-daemon! host-addresses: host-addresses options: ["-pubsub" "-connManager"] wait: 10)

   ;open a client with the given host-address, it will use the daemon already running
  (let* ((c (open-libp2p-client host-addresses: host-addresses wait: 10))
         (self (libp2p-identify c)))
    (for (p (peer-info->string* self))
      (displayln "I am " p)) ;Identify yourself. Print each multiaddr

    (if peer ;If there is a bootstrap peer (connecting command) then connect to it
      (begin
        (displayln "Bootsrap peer found, connecting to it")
        (libp2p-connect c peer)
        (displayln "Connected to Bootstrap peer")))

    ;Subscribe to the chat and spawn a reader thread and writer thread
    (let*-values (((sub cancel) (pubsub-subscribe c "chat"))
           (reader (spawn subscription-reader sub self)))
      (subscription-writer c)
      (thread-abort! reader)))) ;;TODO: Does this exit cleanly?

;;subscription-reader
;;sub: the message channel returned by pubsub-subscribe
;;self: your peer-identity
;;
;;for each message in the message channel, if it is not from you, print it out with the associated Peer ID in front
;;
(def (subscription-reader sub self)

  (for (m sub)

    (if (not (string=? (ID->string (peer-info-id self)) (ID->string (vector-ref m 0)))) ;;compare the message id to your id
      (begin
        (display (ID->string (vector-ref m 0)))
        (display " > ")
        (displayln (bytes->string (vector-ref m 1)))
        (display "> "))
     )))

;;subscription-writer
;;c: the client associated with this node
;;
;;This will read the input from stdin and then publish it to the pubsub topic (with good formatting)
(def (subscription-writer c)
  (let lp ()
    (display "> ")
    (let (line (read-line))
      (unless (eof-object? line)
        (pubsub-publish c "chat" (string->bytes line)) ;;since raw strings cant be sent, you must convert to bytes first
        (lp)))))
