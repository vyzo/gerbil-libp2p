#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p chat example

(import :gerbil/gambit/threads
        :gerbil/gambit/ports
        :std/getopt
        :std/sugar
        :std/iter)
(export main)



;; executable program
(def (main . args)
  (def listen-cmd
    (command 'listen help: "listen for incoming connections"
              (argument 'host-addresses help: "Comma separated multi addresses")))
  (def dial-cmd
    (command 'dial help: "dial an echo peer"
             (argument 'peer  help: "peer to dial")
             (argument 'host-addresses  help: "Comma separated multi addresses")))
  (def help-cmd
    (command 'help help: "display help; help <command> for command help"
             (optional-argument 'command value: string->symbol)))
  (def gopt
    (getopt listen-cmd dial-cmd help-cmd))

  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (let-hash opt
       (case cmd
         ((listen)
          (do-listen .host-addresses))
         ((dial)
          (do-dial .peer .host-addresses))
         ((help)
          (getopt-display-help-topic gopt .?command "libp2p-chat")))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "libp2p-chat" (current-error-port))
     (exit 1))))

(def (do-listen host-addresses)

(displayln host-addresses))




(def (do-dial peer host-addresses)
(displayln peer)
(displayln host-addresses)
)
