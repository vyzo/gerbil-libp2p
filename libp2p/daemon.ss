;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p daemon management

(import :gerbil/gambit/threads
        :gerbil/gambit/ports
        :std/os/pid
        :std/os/signal)
(export #t)

(defstruct daemon (process path)
  final: #t)

(def current-libp2p-daemon
  (make-parameter #f))

;; Starts a new libp2p-daemon only if there is no existing current-libp2p-daemon,
;; and if so sets the current-libp2p-daemon to the new one.
(def (start-the-libp2p-daemon! host-addresses: (host-addrs #f) daemon: (bin "p2pd")
                               options: (args [])
                               address: (sock #f)
                               wait: (timeo 0.4))
  (cond
   ((current-libp2p-daemon)
    => values)
   (else
    (let ((d (start-libp2p-daemon! host-addresses: host-addrs daemon: bin
                                   options: args
                                   address: sock
                                   wait: timeo)))
     (current-libp2p-daemon d)
     d))))

;; Starts a libp2p-daemon without regard to current-libp2p-daemon
(def (start-libp2p-daemon! host-addresses: (host-addrs #f) daemon: (bin "p2pd")
                           options: (args [])
                           address: (sock #f)
                           wait: (timeo 0.4))
    (let* ((path (or sock (string-append "/tmp/p2pd." (number->string (getpid)) ".sock")))
           (addr (string-append "/unix" path))
           (proc (if host-addrs
              (open-process [path: bin arguments: ["-q" "-listen" addr "-hostAddrs" host-addrs args ...]])
              (open-process [path: bin arguments: ["-q" "-listen" addr  args ...]])))
           (d (daemon proc path)))
      (cond
       ((process-status proc timeo #f)
        => (lambda (status)
             (error "p2pd exited prematurely" status))))
      d))

(def (stop-libp2p-daemon! (d (current-libp2p-daemon)))
  (with ((daemon proc path) d)
    (when proc
      (kill (process-pid proc) SIGTERM)
      (let (status (process-status proc))
        (set! (daemon-process d) #f)
        (when (file-exists? path)
          (delete-file path))
        status))))

(def (stop-the-libp2p-daemon!)
  (stop-libp2p-daemon!)
  (current-libp2p-daemon #f))

(def (use-libp2p-daemon! path)
  (current-libp2p-daemon (daemon #f path)))
