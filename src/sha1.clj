(ns sha1
  (:import java.security.MessageDigest))

;; compute SHA1checksum of header+blob
(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))