;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p pubsub api

(import :gerbil/gambit/threads
        :std/sugar
        :std/misc/channel
        :vyzo/libp2p/client
        :vyzo/libp2p/peer
        :vyzo/libp2p/pb/p2pd)
(export #t)

(def (pubsub-get-topics c)
  (let* ((req (Request
               type: 'PUBSUB
               pubsub: (PSRequest
                        type: 'GET_TOPICS)))
         (res (control-request c req Response-pubsub)))
    (PSResponse-topics res)))

(def (pubsub-list-peers c topic)
  (let* ((req (Request
               type: 'PUBSUB
               pubsub: (PSRequest
                        type: 'LIST_PEERS
                        topic: topic)))
         (res (control-request c req Response-pubsub)))
    (map make-ID (PSResponse-peerIDs res))))

(def (pubsub-publish c topic data)
  (let (req (Request
             type: 'PUBSUB
             pubsub: (PSRequest
                      type: 'PUBLISH
                      topic: topic
                      data: data)))
    (control-request c req void)))

(def (pubsub-publisher c topic (bufsz 1024))
  (let (s (open-stream c bufsz))
    (def (publish data)
      (pubsub-publish* s topic data))
    (def (cancel)
      (stream-close s))
    (values publish cancel)))

(def (pubsub-publish* s topic data)
  (let (req (Request
             type: 'PUBSUB
             pubsub: (PSRequest
                      type: 'PUBLISH
                      topic: topic
                      data: data)))
    (do-control-request s req void)))

(def (pubsub-subscribe c topic (bufsz 1024))
  (let* ((s (open-stream c bufsz))
         (req (Request
               type: 'PUBSUB
               pubsub: (PSRequest
                        type: 'SUBSCRIBE
                        topic: topic))))
    (try (do-control-request s req void)
         (catch (e)
           (stream-close s)
           (raise e)))
    (let (ch (make-channel))
      (def (cancel)
        (stream-close s))
      (def (pump s ch)
        (try
         (let lp ()
           (let (msg (stream-read-delimited s bio-read-PSMessage))
             (channel-put ch (vector (ID (PSMessage-from msg))
                                     (PSMessage-data msg)
                                     (PSMessage-seqno msg)
                                     (PSMessage-topicIDs msg)
                                     (PSMessage-signature msg)
                                     (PSMessage-key msg)))
             (lp)))
         (finally
          (channel-close ch))))
      (spawn/name ['pubsub topic] pump s ch)
      (values ch cancel))))
