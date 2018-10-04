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

(def (main . args)
  (def gopt
    (getopt
     (option 'workers "n"
             default: 10
             value: string->number
             help: "number of worker threads")
     (optional-argument 'output
                        default: "dht-crawl.out"
                        help: "output file")))

  (try
   (let (opt (getopt-parse gopt args))
     (let-hash opt
       (start-crawl! .output .workers)))
   (catch (getopt-error? exn)
       (getopt-display-help exn "dht-crawl" (current-error-port))
       (exit 1))))

(def (start-crawl! filename workers)
  (call-with-output-file filename
    (lambda (file)
      (start-logger!)
      (debug "Starting p2pd")
      (start-libp2p-daemon! options: ["-b" "-dhtClient"] wait: 10)
      (debug "Starting indefinite crawl; output to ~a" filename)
      (crawl! file workers))))

(def (crawl! file workers)
  (def c (open-libp2p-client))
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
           (debug "Crawling peer ~a" (ID->string p))
           (crawl-peer p)
           (catch (e)
             (log-error "Error crawling peer" e)
             (write-peer-error p)))))))

  (def (crawl-peer p)
    (let* ((pi (dht-find-peer c p))
           (id (try (identify-peer p) (catch (libp2p-error? e) #f)))
           (pis (try (dht-find-peers-connected-to-peer c p) (catch (libp2p-error? e) []))))
      (unless (null? pis)
        (debug "~a is connected to ~a peers" (ID->string p) (length pis))
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
      (debug "Crawling from ~a" anchor)
      (let (peers (dht-get-closest-peers c anchor 120))
        (if (null? peers)
          (loop)
          (begin
            (debug "Found ~a peers" (length peers))
            (for (p peers)
              (channel-put work p))
            (thread-sleep! 300)
            (loop)))))))
