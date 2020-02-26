(ns idiot
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [sha1 :refer [sha1-sum]])
  (:require [byte-array :as ba])
  (:require [sha])
  (:require [git])
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)
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

;; makes .agit/objects directories
(defn folderMaker []
  (let [objectFolder ".agit/objects/child"]
    (io/make-parents objectFolder))
  (println "Initialized empty Idiot repository in .agit directory"))

;; takes address and slashes it into ../........... form
(defn addressToSlash [address]
  (let [first2Characters (subs address 0 2)
        restOfCharacters (subs address 2)]
    `destination (str ".agit/objects/" first2Characters "/" restOfCharacters)))

;; .agit file maker
(defn doGitInit []
  (cond
    (fileChecker ".agit") (println "Error: .agit directory already exists")
    :else (folderMaker)))

;; init main function
(defn init [args]
  (cond
    (= 1 (count args)) (doGitInit)
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    :else (println "Error: init accepts no arguments")))

;; make header+blob
(defn makeHeaderBlob [fileContents]
  (let [fileLength (count fileContents)]
    (str "blob " fileLength "\000" fileContents)))

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
  (let [zipDestination (addressToSlash blobAddress)]
    (println blobAddress)
    (io/make-parents zipDestination)
    (io/copy (zip-str (makeHeaderBlob fileContents)) (io/file zipDestination))))

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
    (not (fileChecker ".agit")) (println "Error: could not find database. (Did you run `idiot init`?)")
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

;; remove blob heading from unzipper
(defn blobRemover [blobAndContents]
  (let [endNull (+ (s/index-of blobAndContents "\000") 1)]
    (subs blobAndContents endNull)))

;; unzipper of address
(defn addressUnzipper [address]
  (with-open [input (-> (addressToSlash address) io/file io/input-stream)]
    (unzip input)))

;; cat-file main function
(defn cat-file [args]
  (cond
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object")
    (not (fileChecker ".agit")) (println "Error: could not find database. (Did you run `idiot init`?)")
    (not= "-p" (second args)) (println "Error: the -p switch is required")
    (not= 3 (count args)) (println "Error: you must specify an address")
    (not (objectChecker (nth args 2))) (println "Error: that address doesn't exist")
    :else (print (blobRemover (addressUnzipper (nth args 2))))))

;;;;;;;; assignment 2 start refactor above later everything new is below

;; checks to see if a file or directory
(defn isFile [directoryOrFile]
  (.isFile (io/file directoryOrFile)))

;; compute 20 byte address of contents
(defn compute20Bytes [content]
  (sha/bytes (ba/cast (makeHeaderBlob content))))

(defn createTree [directoryOrFile]
  (if (isFile directoryOrFile)
    (git/address "tree" (str "100644 " "file" "\000" (new String (byte-array (compute20Bytes "file contents\n")))))
    (git/address "tree" (str "040000 " directoryOrFile "\000" (new String (byte-array (compute20Bytes "file contents\n")))))))

(defn writeTreeObjects [fileToStore]
  (if (not (objectChecker (git/address "blob" "file contents\n")))
    (addToDatabase "file contents\n" (git/address "blob" "file contents\n"))
    (print "does exist")))

(defn write-wtree [args]
  (cond
    (or (= "-h" (first args)) (= "--help" (first args))) (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
    (not= 0 (count args)) (println "Error: write-wtree accepts no arguments")
    (not (fileChecker ".agit")) (println "Error: could not find database. (Did you run `idiot init`?)")
    :else #_(print (createTree "file.txt"))
          (writeTreeObjects "thing")))

(defn -main [& args]
  (cond
    (= 0 (count args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (or (= "-h" (first args)) (= "--help" (first args))) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "help" (first args)) (help (second args))
    (= "init" (first args)) (init args)
    (= "hash-object" (first args)) (hash-object args)
    (= "cat-file" (first args)) (cat-file args)
    (= "write-wtree" (first args)) (write-wtree (rest args))
    :else (println "Error: invalid command")))