(ns byte-array
  (:refer-clojure :exclude [cast concat]))

(defn cast
  "Return the given value as a byte array. Works for strings or byte arrays."
  [str-or-byte-array]
  (if (string? str-or-byte-array)
    (.getBytes str-or-byte-array)
    str-or-byte-array))

(defn concat
  "Concatenate multiple items into a byte array."
  [& byte-arrays]
  (byte-array
    (mapcat cast byte-arrays)))

(defn- byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [byte-array]
  (->> byte-array
       (map byte->hex-digits)
       (apply str)))