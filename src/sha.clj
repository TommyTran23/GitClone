(ns sha
  (:refer-clojure :exclude [bytes])
  (:require [byte-array :as ba])
  (:import java.security.MessageDigest))

(defn bytes
  "Return the sha1 sum of the data as a 20-byte array."
  [data]
  (.digest (MessageDigest/getInstance "sha1")
           data))

(defn string
  "Return the sha1 sum of the data as a 40-character string."
  [data]
  (-> data bytes ba/to-hex-string))