;;; -*- Gerbil -*-
;;; © vyzo
;;; CID: Content Identifiers

(import :std/net/bio
        :std/protobuf/io
        :std/text/base58
        :std/crypto)
(export #t)

(defstruct CID (v c h)
  final: #t equal: #t)

(defstruct multihash (bytes)
  final: #t equal: #t)

(def Raw #x55)
(def DagProtobuf #x70)

(def (CIDv1 code mh)
  (CID 1 code mh))

(def (CIDv1Raw mh)
  (CID 1 Raw mh))

(def (CIDv1Raw/sha256 content)
  (CIDv1Raw (multihash-sha256 content)))

(def (CID->string cid)
  (if (zero? (CID-v cid))
      (base58-encode (multihash-bytes (CID-h cid)))
      (string-append "z" (base58-encode (CID->bytes cid)))))

(def (string->CID str)
  (cond
   ((string-prefix? str "Qm")
    (CID 0 DagProtobuf (multihash (base58-decode str))))
   ((string-prefix? str "z")
    (bytes->CID (base58-decode (substring str 1 (string-length str)))))
   (else
    (error "Unknown CID encoding" str))))

(def (CID->bytes cid)
  (with ((CID v c h) cid)
    (if (zero? v)
      (multihash-bytes h)
      (let (buf (open-serializer-output-buffer))
        (bio-write-varint v buf)
        (bio-write-varint c buf)
        (bio-write-bytes (multihash-bytes h) buf)
        (chunked-output-u8vector buf)))))

(def (bytes->CID bytes)
  (if (and (= (u8vector-length bytes) 34)
           (eq? (u8vector-ref bytes 0) 18)
           (eq? (u8vector-ref bytes 1) 32))
    (CID 0 DagProtobuf (multihash bytes))
    (let* ((buf (open-input-buffer bytes))
           (v (bio-read-varint buf))
           (c (bio-read-varint buf))
           (rest (bio-input-count buf))
           (bytes (make-u8vector rest))
           (_ (bio-read-bytes bytes buf)))
      (CID v c (multihash bytes)))))

(def SHA2_256 #x12)

(def (multihash-sha256 content)
  (let* ((h (sha256 content))
         (buf (open-serializer-output-buffer)))
    (bio-write-varint SHA2_256 buf)
    (bio-write-varint (u8vector-length h) buf)
    (bio-write-bytes h buf)
    (multihash (chunked-output-u8vector buf))))

(def (multihash->string mh)
  (base58-encode (multihash-bytes mh)))

(def (string->multihash str)
  (multihash (base58-decode str)))
