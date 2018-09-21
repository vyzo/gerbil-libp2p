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
(export #t)

(defstruct (libp2p-error <error>) ())

(def (raise-libp2p-error where what . irritants)
  (raise (make-libp2p-error what irritants where)))

(defstruct client (d mx handlers path sock thread)
  final: #t)

(defstruct stream (sock in out info)
  final: #t)

(def (stream-close s)
  (when (stream-sock s)
    (ssocket-close (stream-sock s))
    (set! (stream-sock s) #f)))

(defmethod {destroy stream}
  stream-close)

(def (stream-write-delimited s obj bio-write-e)
  (bio-write-delimited obj bio-write-e (stream-out s))
  (bio-force-output (stream-out s)))

(def (stream-read-delimited s bio-read-e)
  (bio-read-delimited bio-read-e (stream-in s)))

(def (open-libp2p-client (path #f))
  (let (d (start-libp2p-daemon!))
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


(def (libp2p-connect c pinfo)
  (let (req
        (Request
         type: 'CONNECT
         connect: (ConnectRequest
                   peer: (ID-bytes (peer-info-id pinfo))
                   addrs: (map multiaddr-bytes (peer-info-addrs pinfo)))))
    (control-request c req void)))

(def (libp2p-stream c peer protos (bufsz 4096))
  (let* ((id (if (ID? peer) peer (peer-info-id peer)))
         (s (open-stream c bufsz))
         (req
          (Request
           type: 'STREAM_OPEN
           streamOpen: (StreamOpenRequest
                        peer: (ID-bytes id)
                        proto: protos)))
         (res
          (do-control-request s req Response-streamInfo))
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
        (let (req
              (Request
               type: 'STREAM_HANDLER
               streamHandler: (StreamHandlerRequest
                               path: (client-path c)
                               proto: protos)))
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
         (info (stream-read-delimited s bio-read-StreamInfo))
         (info (cons (StreamInfo-proto info)
                     (peer-info (ID (StreamInfo-peer info))
                                [(multiaddr (StreamInfo-addr info))]))))
    (set! (stream-info s)
      info)
    (cond
     ((with-lock (client-mx c) (cut hash-get (client-handlers c) (car info)))
      => (lambda (handler)
           (make-will s stream-close)
           (dispatch-handler handler s)))
     (else
      (warning "Incoming stream for unknown protocol: ~a" (car info))
      (stream-close s)))))

(def (dispatch-handler handler s)
  (try
   (handler s)
   (catch (e)
     (log-error "Unhandled exception" e)
     (stream-close s))))


(def (libp2p-close c)
  (with ((client _ mx _ _ sock thread) c)
    (with-lock mx
      (when sock
        (ssocket-close sock)
        (thread-group-kill! (thread-thread-group thread))
        (set! (client-sock c) #f)
        (set! (client-thread c) #f)))))
