;;; -*- Gerbil -*-
;;; © vyzo
;;; libp2p multiaddrs

(import :std/net/address
        :std/net/bio
        :std/protobuf/io
        :std/text/utf8
        :std/text/base58)
(export #t)

(defstruct multiaddr (bytes)
  final: #t equal: #t)

(def (multiaddr->string ma)
  (let (buf (open-input-buffer (multiaddr-bytes ma)))
    (let lp ((parts []))
      (if (eof-object? (bio-peek-u8 buf))
        (string-join (cons "" (reverse parts)) #\/)
        (let (code (bio-read-varint buf))
          (cond
           ((protocol-by-code code)
            => (lambda (proto)
                 (let ((parts (cons (protocol-name proto) parts))
                       (size (protocol-size proto)))
                   (cond
                    ((fxzero? size)
                     (lp parts))
                    ((fxpositive? size)
                     (let (bytes (make-u8vector size))
                       (bio-read-bytes bytes buf)
                       (let (str (protocol-bytes->string proto bytes))
                         (lp (cons str parts)))))
                    (else
                     (let* ((size (bio-read-varint buf))
                            (bytes (make-u8vector size)))
                       (bio-read-bytes bytes buf)
                       (let (str (protocol-bytes->string proto bytes))
                         (lp (cons str parts)))))))))
           (else
            (error "Unknown protocol code" code))))))))

(def (string->multiaddr str)
  (let (parts (string-split str #\/))
    (unless (string-empty? (car parts))
      (error "Invalid multiaddr" str))
    (let (buf (open-serializer-output-buffer))
      (let lp ((rest (cdr parts)))
        (match rest
          ([part . rest]
           (cond
            ((protocol-by-name part)
             => (lambda (proto)
                  (bio-write-varint (protocol-code proto) buf)
                  (let (size (protocol-size proto))
                    (cond
                     ((fxzero? size)
                      (lp rest))
                     ((fxpositive? size)
                      (if (protocol-path? proto)
                        (let* ((str (string-join (cons "" rest) #\/))
                               (bytes (protocol-string->bytes proto str)))
                          (bio-write-bytes bytes buf)
                          (lp []))
                        (match rest
                          ([part . rest]
                           (let (bytes (protocol-string->bytes proto part))
                             (bio-write-bytes bytes buf)
                             (lp rest))))))
                     (else
                      (if (protocol-path? proto)
                        (let* ((str (string-join (cons "" rest) #\/))
                               (bytes (protocol-string->bytes proto str)))
                          (bio-write-delimited-bytes bytes buf)
                          (lp []))
                        (match rest
                          ([part . rest]
                           (let (bytes (protocol-string->bytes proto part))
                             (bio-write-delimited-bytes bytes buf)
                             (lp rest))))))))))
            (else
             (error "Unknown protocol" part))))
          (else
           (multiaddr (chunked-output-u8vector buf))))))))


(defstruct protocol (name code size path? bytes-e string-e)
  final: #t print: (name))

(def (protocol-by-name name)
  (hash-get +protocols-by-name+ name))

(def (protocol-by-code code)
  (hash-get +protocols-by-code+ code))

(def (protocol-string->bytes proto str)
  ((protocol-bytes-e proto) str))

(def (protocol-bytes->string proto bytes)
  ((protocol-string-e proto) bytes))

;;; protocol definitions
(def +protocols-by-name+ (make-hash-table))
(def +protocols-by-code+ (make-hash-table-eq))

(defrules defprotocol ()
  ((_ name code size path? bytes-e string-e)
   (let (proto (protocol name code size path? bytes-e string-e))
     (hash-put! +protocols-by-name+ name proto)
     (hash-put! +protocols-by-code+ code proto))))

(def P_IP4     #x0004)
(def P_TCP     #x0006)
(def P_UDP     #x0111)
(def P_DCCP    #x0021)
(def P_IP6     #x0029)
(def P_QUIC    #x01CC)
(def P_SCTP    #x0084)
(def P_UDT     #x012D)
(def P_UTP     #x012E)
(def P_UNIX    #x0190)
(def P_P2P     #x01A5)
(def P_HTTP    #x01E0)
(def P_HTTPS   #x01BB)
(def P_ONION   #x01BC)
(def P_WS      #x01DD)
(def P_WSS     #x01DE)
(def P_CIRCUIT #x0122)
(def P_DNS4    #x0036)
(def P_DNS6    #x0037)
(def P_DNSADDR #x0038)

(def (port-string->bytes str)
  (let (port (string->number str))
    (unless (< port 65536)
      (error "illegal port" port))
    (let (bytes (make-u8vector 2))
      (##u8vector-set! bytes 0 (##fxand (##fxarithmetic-shift-right port 8) #xff))
      (##u8vector-set! bytes 1 (##fxand port #xff))
      bytes)))

(def (port-bytes->string bytes)
  (number->string
   (##fxior (##fxarithmetic-shift-left (##u8vector-ref bytes 0) 8)
            (##u8vector-ref bytes 1))))

(defprotocol "ip4" P_IP4 4 #f string->ip4-address ip4-address->string)
(defprotocol "ip6" P_IP6 16 #f string->ip6-address ip6-address->string)
(defprotocol "tcp" P_TCP 2 #f port-string->bytes port-bytes->string)
(defprotocol "ws"  P_WS 0 #f void void)
(defprotocol "wss" P_WSS 0 #f void void)
(defprotocol "quic" P_QUIC 0 #f void void)
(defprotocol "p2p-circuit" P_CIRCUIT 0 #f void void)
(defprotocol "p2p" P_P2P -1 #f base58-decode base58-encode)
(defprotocol "dns4" P_DNS4 -1 #f string->utf8 utf8->string)
(defprotocol "dns6" P_DNS6 -1 #f string->utf8 utf8->string)
(defprotocol "dnsaddr" P_DNSADDR -1 #f string->utf8 utf8->string)
(defprotocol "unix" P_UNIX -1 #t string->utf8 utf8->string)
