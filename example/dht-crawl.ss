;;; -*- Gerbil -*-
;;; © vyzo
;;; A simple (IPFS) DHT crawler

(import :gerbil/gambit/threads
        :gerbil/gambit/ports
        :std/misc/channel
        :std/iter
        :std/getopt
        :std/logger
        :std/sugar
        :std/crypto
        :std/text/base64
        :vyzo/libp2p
        :vyzo/libp2p/pb/identify)
(export main)

(deflogger libp2p)

(def (main . args)
  (def gopt
    (getopt
     (option 'workers "n"
             default: 10
             value: string->number
             help: "number of worker threads")
    (argument 'host-addresses help: "Comma separated multi addresses")
     (optional-argument 'output
                        default: "dht-crawl.out"
                        help: "output file")))

  (try
   (let (opt (getopt-parse gopt args))
     (let-hash opt
       (start-crawl! .output .workers .host-addresses)))
   (catch (getopt-error? exn)
       (getopt-display-help exn "dht-crawl" (current-error-port))
       (exit 1))))

(def (start-crawl! filename workers addresses)
  (call-with-output-file filename
    (lambda (file)
      (start-logger!)
      (debugf "Starting p2pd")
      (start-libp2p-daemon! host-addresses: addresses options: ["-b" "-dhtClient" "-connManager"] wait: 10)
      (debugf "Starting indefinite crawl; output to ~a" filename)
      (crawl! file workers addresses))))

(def (crawl! file workers addresses)
  (def c (open-libp2p-client addresses))
  (def mx-out (make-mutex 'output))
  (def mx-peers (make-mutex 'peers))
  (def peers (make-hash-table))
  (def work (make-channel))

  (def (worker)
    (for (p work)
      (mutex-lock! mx-peers)
      (if (hash-get peers p)
        (mutex-unlock! mx-peers)
        (begin
          (hash-put! peers p #t)
          (mutex-unlock! mx-peers)
          (try
           (debugf "Crawling peer ~a" (ID->string p))
           (crawl-peer p)
           (catch (e)
             (errorf "Error crawling peer: ~a" e)
             (write-peer-error p)))))))

  (def (crawl-peer p)
    (let* ((pi (dht-find-peer c p))
           (id (try (identify-peer p) (catch (libp2p-error? e) #f)))
           (pis (try (dht-find-peers-connected-to-peer c p) (catch (libp2p-error? e) []))))
      (unless (null? pis)
        (debugf "~a is connected to ~a peers" (ID->string p) (length pis))
        (for (pi pis)
          (channel-put work (peer-info-id pi))))
      (write-peer p id pi pis)))

  (def (identify-peer p)
    (let (s (libp2p-stream c p ["/ipfs/id/1.0.0"]))
      (with-destroy s
        (stream-read-delimited s bio-read-Identify))))

  (def (write-peer p id pi pis)
    (write-crawl-entry
     [(ID->string p)
      (if id (cons (Identify-agentVersion id) (Identify-protocols id)) 'ERROR)
      (map multiaddr->string (peer-info-addrs pi))
      (map ID->string (map peer-info-id pis))]))

  (def (write-crawl-entry entry)
    (with-lock mx-out
      (lambda ()
        (write entry file)
        (newline file)
        (force-output file))))

  (def (write-peer-error p)
    (write-crawl-entry [(ID->string p) 'ERROR]))

  (for (_ (in-range workers))
    (spawn worker))

  (let loop ()
    (let (anchor (base64-encode (random-bytes 32)))
      (debugf "Crawling from ~a" anchor)
      (let (peers (try
                   (dht-get-closest-peers c anchor timeout: 120)
                   (catch (libp2p-error? e)
                     (errorf "Crawl error: ~a" e)
                     [])))
        (if (null? peers)
          (begin
            (thread-sleep! 10)
            (loop))
          (begin
            (debugf "Found ~a peers" (length peers))
            (for (p peers)
              (channel-put work p))
            (thread-sleep! 300)
            (loop)))))))
