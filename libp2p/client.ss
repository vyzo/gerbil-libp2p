;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p daemon management

(import :gerbil/gambit/threads
        :gerbil/gambit/misc
        :std/sugar
        :std/error
        :std/net/socket
        :std/net/bio
        :std/protobuf/io
        :std/os/pid
        :std/misc/threads
        :std/logger
        :vyzo/libp2p/daemon
        :vyzo/libp2p/peer
        :vyzo/libp2p/multiaddr
        :vyzo/libp2p/pb/p2pd)
(export (except-out #t errorf warnf infof debugf verbosef))

(deflogger libp2p)

(defstruct (libp2p-error <error>) ())

(def (raise-libp2p-error where what . irritants)
  (raise (make-libp2p-error what irritants where)))

(defstruct client (d mx handlers path sock thread)
  final: #t)

(defstruct stream (sock in out info)
  final: #t)

(def (stream-close s)
  (alet (sock (stream-sock s))
    (ssocket-close sock)
    (set! (stream-sock s) #f)
    (void)))

(def (stream-close-output s)
  (alet (sock (stream-sock s))
    (ssocket-close-output sock #t)))

(defmethod {destroy stream}
  stream-close)

(def (stream-write-delimited s obj bio-write-e)
  (bio-write-delimited obj bio-write-e (stream-out s))
  (bio-force-output (stream-out s)))

(def (stream-read-delimited s bio-read-e)
  (bio-read-delimited bio-read-e (stream-in s)))

(defrules with-error-stream-close ()
  ((_ s body ...)
   (try body ...
        (catch (e)
          (stream-close s)
          (raise e)))))

(def (open-libp2p-client host-addresses: (host-addresses #f) options: (args [])  address: (sock #f)  wait: (timeo 12) (path #f)) ;; Extra arguments host-address and options
  (let (d (start-the-libp2p-daemon! host-addresses: host-addresses options: args address: sock wait: timeo)) ;; Should go with host-address/tranpsort/port
    (make-client d (make-mutex 'libp2p-client) (make-hash-table) path #f #f)))

(def (open-stream c bufsz)
  (let (sock (ssocket-connect (daemon-path (client-d c))))
    (make-stream sock
                 (open-ssocket-input-buffer sock bufsz)
                 (open-ssocket-output-buffer sock bufsz)
                 #f)))

(def (control-request c req response-e)
  (let (s (open-stream c 256))
    (with-destroy s
      (do-control-request s req response-e))))

(def (do-control-request s req response-e)
  (stream-write-delimited s req bio-write-Request)
  (let (res (stream-read-delimited s bio-read-Response))
    (case (Response-type res)
      ((OK)
       (response-e res))
      ((ERROR)
       (raise-libp2p-error 'libp2p-request (ErrorResponse-msg (Response-error res))))
      (else
       (error "Unexpected response type" (Response-type res))))))

(def (libp2p-identify c)
  (let (res (control-request c (Request type: 'IDENTIFY) Response-identify))
    (peer-info (ID (IdentifyResponse-id res))
               (map make-multiaddr (IdentifyResponse-addrs res)))))


(def (libp2p-connect c pinfo timeout: (timeout #f))
  (let (req
        (Request
         type: 'CONNECT
         connect: (ConnectRequest
                   peer: (ID-bytes (peer-info-id pinfo))
                   addrs: (map multiaddr-bytes (peer-info-addrs pinfo))
                   timeout: (request-timeout timeout))))
    (control-request c req void)))

(def (libp2p-disconnect c p)
  (let (req
        (Request
         type: 'DISCONNECT
         disconnect: (DisconnectRequest
                      peer: (ID-bytes p))))
    (control-request c req void)))

(def (libp2p-stream c peer protos buffer: (bufsz 4096) timeout: (timeout #f))
  (let* ((id (if (ID? peer) peer (peer-info-id peer)))
         (s (open-stream c bufsz))
         (req
          (Request
           type: 'STREAM_OPEN
           streamOpen: (StreamOpenRequest
                        peer: (ID-bytes id)
                        proto: protos
                        timeout: (request-timeout timeout))))
         (res
          (with-error-stream-close s
            (do-control-request s req Response-streamInfo)))
         (info
          (cons (StreamInfo-proto res)
                (peer-info id [(multiaddr (StreamInfo-addr res))]))))
    (set! (stream-info s)
      info)
    (make-will s stream-close)
    s))

(def (libp2p-listen c protos handler)
  (with ((client _ mx handlers path sock) c)
    (with-lock mx
      (lambda ()
        (unless sock
          (let* ((path
                  (or path
                      (string-append "/tmp/p2pd.client." (number->string (getpid)) ".sock")))
                 (_ (when (file-exists? path) (delete-file path)))
                 (sock
                  (ssocket-listen path)))
            (set! (client-path c) path)
            (set! (client-sock c) sock)
            (set! (client-thread c)
              (spawn/group 'libp2p client-accept c))))
        (let* ((path (client-path c))
               (addr (string-append "/unix" path))
               (maddr (string->multiaddr addr))
               (req
                (Request
                 type: 'STREAM_HANDLER
                 streamHandler: (StreamHandlerRequest
                                 addr: (multiaddr-bytes maddr)
                                 proto: protos))))
          (control-request c req void)
          (for-each (cut hash-put! handlers <> handler)
                    protos))))))

(def (client-accept c)
  (let (sock (client-sock c))
    (while #t
      (let (cli (ssocket-accept sock))
        (spawn/name 'libp2p-stream client-dispatch c cli)))))

(def (client-dispatch c sock)
  (let* ((s (make-stream sock
                       (open-ssocket-input-buffer sock)
                       (open-ssocket-output-buffer sock)
                       #f))
         (_ (make-will s stream-close))
         (info (stream-read-delimited s bio-read-StreamInfo))
         (info (cons (StreamInfo-proto info)
                     (peer-info (ID (StreamInfo-peer info))
                                [(multiaddr (StreamInfo-addr info))]))))
    (set! (stream-info s)
      info)
    (cond
     ((with-lock (client-mx c) (cut hash-get (client-handlers c) (car info)))
      => (lambda (handler)

           (dispatch-handler handler s)))
     (else
      (warnf "Incoming stream for unknown protocol: ~a" (car info))
      (stream-close s)))))

(def (dispatch-handler handler s)
  (try
   (handler s)
   (catch (e)
     (errorf "Unhandled exception: ~a" e)
     (stream-close s))))


(def (libp2p-close c)
  (with ((client _ mx _ _ sock thread) c)
    (with-lock mx
      (lambda ()
        (when sock
          (ssocket-close sock)
          (thread-group-kill! (thread-thread-group thread))
          (set! (client-sock c) #f)
          (set! (client-thread c) #f))))))

(def (libp2p-list-peers c)
  (let (res (control-request c (Request type: 'LIST_PEERS) Response-peers))
    (map pb->peer-info res)))

(def (pb->peer-info pi)
  (peer-info (ID (PeerInfo-id pi))
             (map make-multiaddr (PeerInfo-addrs pi))))

(def (request-timeout timeout)
  (and timeout (inexact->exact (round timeout))))
