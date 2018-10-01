;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p DHT api

(import :gerbil/gambit/threads
        :std/sugar
        :std/iter
        :std/misc/channel
        :vyzo/libp2p/client
        :vyzo/libp2p/peer
        :vyzo/libp2p/multiaddr
        :vyzo/libp2p/pb/p2pd)
(export #t)

(defrules XXX ()
  (_ (error "XXX Implement me!")))

(def (libp2p-dht-find-peer c p (timeout #f))
  (let* ((req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'FIND_PEER
                     peer: (ID-bytes p)
                     timeout: (request-timeout timeout))))
         (res (control-request c req Response-dht)))
    (pb->peer-info (DHTResponse-peer res))))

(def (libp2p-dht-find-peers-connected-to-peer* c p (timeout #f))
  (let* ((s (open-stream c 1024))
         (req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'FIND_PEERS_CONNECTED_TO_PEER
                     peer: (ID-bytes p)
                     timeout: (request-timeout timeout))))
         (_ (with-error-stream-close s (do-control-request s req void)))
         (ch (make-channel)))
    (spawn dht-channel-pump ch s (lambda (r) (pb->peer-info (DHTResponse-peer r))))
    ch))

(def (libp2p-dht-find-peers-connected-to-peer c p (timeout #f))
  (let (ch (libp2p-dht-find-peers-connected-to-peer* c p timeout))
    (for/collect (v ch) v)))

(def (libp2p-dht-get-closest-peers* c key (timeout #f))
  (let* ((s (open-stream c 1024))
         (req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'GET_CLOSEST_PEERS
                     key: key
                     timeout: (request-timeout timeout))))
         (_ (with-error-stream-close s (do-control-request s req void)))
         (ch (make-channel)))
    (spawn dht-channel-pump ch s (lambda (r) (ID (DHTResponse-value r))))
    ch))

(def (libp2p-dht-get-closest-peers c key (timeout #f))
  (let (ch (libp2p-dht-get-closest-peers* c key timeout))
    (for/collect (v ch) v)))

(def (libp2p-dht-get-public-key c p (timeout #f))
  (let* ((req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'GET_PUBLIC_KEY
                     peer: (ID-bytes p)
                     timeout: (request-timeout timeout))))
         (res (control-request c req Response-dht)))
    (DHTResponse-value res)))

(def (libp2p-dht-get-value c key (timeout #f))
  (let* ((req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'GET_VALUE
                     key: key
                     timeout: (request-timeout timeout))))
         (res (control-request c req Response-dht)))
    (DHTResponse-value res)))

(def (libp2p-dht-search-value* c key (timeout #f))
  (let* ((s (open-stream c 1024))
         (req (Request
               type: 'DHT
               dht: (DHTRequest
                     type: 'SEARCH_VALUE
                     key: key
                     timeout: (request-timeout timeout))))
         (_ (with-error-stream-close s (do-control-request s req void)))
         (ch (make-channel)))
    (spawn dht-channel-pump ch s DHTResponse-value)
    ch))

(def (libp2p-dht-search-value c key (timeout #f))
  (for (ch (libp2p-dht-search-value* c key timeout))
    (for/collect (v ch) v)))

(def (libp2p-dht-put-value c key val (timeout #f))
  (let (req (Request
             type: 'DHT
             dht: (DHTRequest
                   type: 'PUT_VALUE
                   key: key
                   value: val
                   timeout: (request-timeout timeout))))
    (control-request c req void)))

(def (libp2p-dht-find-providers* c cid (count #f) (timeout #f))
  XXX
  )

(def (libp2p-dht-find-providers c cid (count #f) (timeout #f))
  (for (ch (libp2p-dht-find-providers* c cid count timeout))
    (for/collect (v ch) v)))

(def (libp2p-dht-provide c cid (timeout #f))
  XXX
  )

;; utilities
(def (request-timeout timeout)
  (and timeout (inexact->exact (floor (* timeout 1000000000)))))

(def (dht-channel-pump ch s value-e)
  (with-destroy s
    (with-destroy ch
      (let lp ()
        (let (next (stream-read-delimited s bio-read-DHTResponse))
          (when (eq? (DHTResponse-type next) 'VALUE)
            (channel-put ch (value-e next))
            (lp)))))))

(defrules with-error-stream-close ()
  ((_ s body ...)
   (try body ...
        (catch (e)
          (stream-close s)
          (raise e)))))
