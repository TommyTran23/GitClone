(ns idiot
  (:require [clojure.java.io :as io])
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream InflaterInputStream)))

;; help command
(defn help [args]
  (cond
    (= 0 (count args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (or (= "-h" args) (= "--help" args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "init" args) (println "Usage: idiot init")
    (= "help" args) (println "Usage: idiot help <command>")
    (= "hash-object" args) (println "Usage: idiot hash-object [-w] <file>")
    :else (println "Error: invalid command\n")))

;; checks if file exists
(defn fileChecker [args]
  (.exists (io/file args)))

;; makes .git/objects directories
(defn folderMaker []
  (let [objectFolder ".agit/objects/child"]
    (io/make-parents objectFolder))
  (println "Initialized empty Idiot repository in .git directory\n"))

;; .git file maker
(defn doGitInit []
  (cond
    (fileChecker ".agit") (println "Error: .git directory already exists\n")
    :else (folderMaker)))

;; init main function
(defn init [args]
  (cond
    (= 1 (count args)) (doGitInit)
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    :else (println "Error: init accepts no arguments\n")))

;; make header+blob
(defn makeHeaderBlob [file]
  (let [fileLength (count file)]
    (str "blob " fileLength "\000" file))
  )

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

;; zip files
(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

;; generate SHA1 of file contents
(defn shaOfFile [file]
  (sha1-sum (makeHeaderBlob (slurp file))))

(defn addToDatabase [fileContents blobAddress]
  (let [first2Characters (subs blobAddress 0 2)
        restOfCharacters (subs blobAddress 2)
        zipDestination (str ".agit/objects/" first2Characters "/" restOfCharacters)]
    (println blobAddress)
    (io/make-parents zipDestination)
    (io/copy (zip-str fileContents) (io/file zipDestination)))
  )

;; check w flag
(defn wFlag [args]
  (cond
    (not (= 3 (count args))) (println "Error: you must specify a file.\n")
    (not (fileChecker (nth args 2))) (println "Error: that file isn't readable\n")
    :else (addToDatabase (slurp (nth args 2)) (shaOfFile (nth args 2)))))

;; hash object main function
(defn hash-object [args]
  (cond
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    (= "-w" (second args)) (wFlag args)
    (not (fileChecker ".agit")) (println "Error: could not find database. (Did you run `idiot init`?)")
    (= 1 (count args)) (println "Error: you must specify a file.\n")
    (not (fileChecker (second args))) (println "Error: that file isn't readable\n")
    (not (.isFile (io/file (second args)))) (println "Error: that file isn't readable\n")
    :else (println (shaOfFile (second args)))
    ))

;; check if object exists
(defn objectChecker [address]
  (let [first2Characters (subs address 0 2)
        restOfCharacters (subs address 2)
        objectFile (str ".agit/objects/" first2Characters "/" restOfCharacters)]
    (.isFile (io/file objectFile))))

(defn addressToSlash [address]
  (let [first2Characters (subs address 0 2)
        restOfCharacters (subs address 2)]
    `destination (str ".agit/objects/" first2Characters "/" restOfCharacters))
  )

;; unzip files
(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

;; unzipper of address
(defn addressUnzipper [address]
  (with-open [input (-> (addressToSlash address) io/file io/input-stream)]
    (unzip input)))

;; cat-file main function
(defn cat-file [args]
  (cond
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object")
    (not (fileChecker ".agit")) (println "Error: could not find database. (Did you run `idiot init`?)")
    (not= "-p" (second args)) (println "Error: the -p switch is required\n")
    ;(not (or (= 1 (count args) (= 3 (count args))))) (println "Error: you must specify an address.\n")
    (not= 3 (count args)) (println "Error: you must specify an address.\n")
    (not (objectChecker (nth args 2))) (println "Error: that address doesn't exist\n")
    :else (println (addressUnzipper (nth args 2)))
    ))

(defn -main [& args]
  (cond
    (= 0 (count args)) (println "nothing inputted change this later")
    (= "help" (first args)) (help (second args))
    (= "init" (first args)) (init args)
    (= "hash-object" (first args)) (hash-object args)
    (= "cat-file" (first args)) (cat-file args)
    :else (println "Error: invalid command\n")
    ))