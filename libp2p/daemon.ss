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

(def (start-libp2p-daemon! daemon: (bin "p2pd")
                           options: (args [])
                           address: (sock #f))
  (cond
   ((current-libp2p-daemon)
    => values)
   (else
    (let* ((sock (or sock (string-append "/tmp/p2pd." (number->string (getpid)) ".sock")))
           (proc (open-process [path: bin arguments: ["-q" "-sock" sock args ...]]))
           (d (daemon proc sock)))
      (cond
       ((process-status proc 1 #f)
        => (lambda (status)
             (error "p2pd exited prematurely" status))))
      (current-libp2p-daemon d)
      d))))

(def (stop-libp2p-daemon! (d (current-libp2p-daemon)))
  (with ((daemon proc path) d)
    (when proc
      (kill (process-pid proc) SIGTERM)
      (let (status (process-status proc))
        (set! (daemon-process d) #f)
        (when (file-exists? path)
          (delete-file path))
        status))))

(def (use-libp2-daemon! path)
  (current-libp2p-daemon (daemon #f path)))
