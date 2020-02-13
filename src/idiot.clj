(ns idiot
  (:require [clojure.java.io :as io])
  (:import java.security.MessageDigest
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream InflaterInputStream)))

;; help command
(defn help [args]
  (cond
    (= 0 (count args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (or (= "-h" args) (= "--help" args)) (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "init" args) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    (= "help" args) (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "hash-object" args) (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    (= "cat-file" args) (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object")
    :else (println "Error: invalid command")))

;; checks if file exists
(defn fileChecker [fileToCheck]
  (.exists (io/file fileToCheck)))

;; makes .git/objects directories
(defn folderMaker []
  (let [objectFolder ".git/objects/child"]
    (io/make-parents objectFolder))
  (println "Initialized empty Idiot repository in .git directory"))

;; takes address and slashes it into ../........... form
(defn addressToSlash [address]
  (let [first2Characters (subs address 0 2)
        restOfCharacters (subs address 2)]
    `destination (str ".git/objects/" first2Characters "/" restOfCharacters)))

;; .git file maker
(defn doGitInit []
  (cond
    (fileChecker ".git") (println "Error: .git directory already exists")
    :else (folderMaker)))

;; init main function
(defn init [args]
  (cond
    (= 1 (count args)) (doGitInit)
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    :else (println "Error: init accepts no arguments")))

;; make header+blob
(defn makeHeaderBlob [file]
  (let [fileLength (count file)]
    (str "blob " fileLength "\000" file)))

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

;; adds zipped file contents into stored objects database
(defn addToDatabase [fileContents blobAddress]
  (let [zipDestination (addressToSlash blobAddress)
        blobFileContents (makeHeaderBlob fileContents)]
    (println blobAddress)
    (io/make-parents zipDestination)
    (io/copy (zip-str blobFileContents) (io/file zipDestination))))

;; check w flag
(defn wFlag [args]
  (cond
    (not (= 3 (count args))) (println "Error: you must specify a file.")
    (not (fileChecker (nth args 2))) (println "Error: that file isn't readable")
    :else (addToDatabase (slurp (nth args 2)) (shaOfFile (nth args 2)))))

;; hash object main function
(defn hash-object [args]
  (cond
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    (= "-w" (second args)) (wFlag args)
    (not (fileChecker ".git")) (println "Error: could not find database. (Did you run `idiot init`?)")
    (= 1 (count args)) (println "Error: you must specify a file.")
    (not (fileChecker (second args))) (println "Error: that file isn't readable")
    (not (.isFile (io/file (second args)))) (println "Error: that file isn't readable")
    :else (println (shaOfFile (second args)))))

;; check if object exists
(defn objectChecker [address]
  (.isFile (io/file (addressToSlash address))))

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
    (not (fileChecker ".git")) (println "Error: could not find database. (Did you run `idiot init`?)")
    (not= "-p" (second args)) (println "Error: the -p switch is required")
    (not= 3 (count args)) (println "Error: you must specify an address")
    (not (objectChecker (nth args 2))) (println "Error: that address doesn't exist")
    :else (print (addressUnzipper (nth args 2)))))

(defn -main [& args]
  (cond
    (= 0 (count args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (or (= "-h" (first args)) (= "--help" (first args))) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "help" (first args)) (help (second args))
    (= "init" (first args)) (init args)
    (= "hash-object" (first args)) (hash-object args)
    (= "cat-file" (first args)) (cat-file args)
    :else (println "Error: invalid command")))