;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p daemon management

(import :gerbil/gambit/threads
        :std/sugar
        :std/error
        :std/net/socket
        :std/net/bio
        :std/protobuf/io
        :vyzo/libp2p/daemon
        :vyzo/libp2p/peer
        :vyzo/libp2p/multiaddr
        :vyzo/libp2p/pb/p2pd)
(export #t)

(defstruct (libp2p-error <error>) ())

(def (raise-libp2p-error where what . irritants)
  (raise (make-libp2p-error what irritants where)))

(defstruct client (d mx sock handlers)
  final: #t)

(defstruct stream (sock in out info)
  final: #t)

(defmethod {destroy stream}
  (lambda (self)
    (ssocket-close (stream-sock self))))

(def (stream-write-delimited s obj bio-write-e)
  (bio-write-delimited obj bio-write-e (stream-out s))
  (bio-force-output (stream-out s)))

(def (stream-read-delimited s bio-read-e)
  (bio-read-delimited bio-read-e (stream-in s)))

(def (open-libp2p-client)
  (let (d (start-libp2p-daemon!))
    (make-client d (make-mutex 'libp2p-client) #f (make-hash-table))))

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
  (stream-write-delimited s (Request type: 'IDENTIFY) bio-write-Request)
  (let (res (stream-read-delimited s bio-read-Response))
    (case (Response-type res)
      ((OK)
       (response-e res))
      ((ERROR)
       (raise-libp2p-error 'libp2p-identify (ErrorResponse-msg (Response-error res))))
      (else
       (error "Unexpected response type" (Response-type res))))))

(def (libp2p-identify c)
  (let (res (control-request c (Request type: 'IDENTIFY) Response-identify))
    (peer-info (ID (IdentifyResponse-id res))
               (map make-multiaddr (IdentifyResponse-addrs res)))))
