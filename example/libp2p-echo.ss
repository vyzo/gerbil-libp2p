;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p echo example

(import :gerbil/gambit/threads
        :std/getopt
        :std/sugar
        :std/iter
        :std/logger
        :std/net/bio
        :vyzo/libp2p)
(export main)

(def echo-proto "/echo/1.0.0")

(def (register-echo-handler! c)
  (libp2p-listen c [echo-proto] echo-handler))

;; the echo protocol [see https://github.com/libp2p/go-libp2p-examples/tree/master/echo]
;; reads a line of text, echoes it back, and closes the stream
;; this implementation reads up to 4k chars because I don't like unbounded reads.
(def (echo-handler s)
  (debug "New stream from ~a" (peer-info->string (cdr (stream-info s))))
  (let (line (bio-read-line (stream-in s) #\newline #t 4096))
    (unless (eof-object? line)
      (bio-write-string line (stream-out s))
      (bio-force-output (stream-out s))))
  (stream-close s))

;; echo client -- sends a message followed by a newline, and reads back the response
(def (do-echo c peer what)
  (libp2p-connect c peer)
  (let (s (libp2p-stream c peer [echo-proto]))
    (with-destroy s
      (bio-write-string what (stream-out s))
      (bio-write-char #\newline (stream-out s))
      (bio-force-output (stream-out s))
      (bio-read-line (stream-in s)))))

;; executable program
(def (main . args)
  (def listen-cmd
    (command 'listen help: "listen for incoming connections"))
  (def dial-cmd
    (command 'dial help: "dial an echo peer"
             (argument 'peer value: string->peer-info help: "peer to dial")
             (argument 'msg help: "what to say to the peer")))
  (def help-cmd
    (command 'help help: "display help; help <command> for command help"
             (optional-argument 'command value: string->symbol)))
  (def gopt
    (getopt listen-cmd dial-cmd help-cmd))

  (start-logger!)
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (let-hash opt
       (case cmd
         ((listen)
          (do-listen))
         ((dial)
          (do-dial .peer .msg))
         ((help)
          (getopt-display-help-topic gopt .?command "libp2p-echo")))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "libp2p-echo" (current-error-port))
     (exit 1))))

(def (do-listen)
  (let* ((c (open-libp2p-client))
         (self (libp2p-identify c)))
    (for (p (peer-info->string* self))
      (displayln "I am " p))
    (libp2p-listen c [echo-proto] echo-handler)
    (thread-sleep! +inf.0)))

(def (do-dial peer what)
  (let* ((c (open-libp2p-client))
         (reply (do-echo c peer what)))
    (displayln (peer-info->string peer) " says: " what)))
