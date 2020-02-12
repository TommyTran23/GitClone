(ns idiot
  (:require [clojure.java.io :as io]))

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
(defn fileChecker []
  (.exists (io/file ".agit")))

;; makes .git/objects directories
(defn folderMaker []
  (let [objectFolder ".agit/objects/child"]
    (io/make-parents objectFolder))
  (println "Initialized empty Idiot repository in .git directory\n"))

;; .git file maker
(defn gitInit []
  (cond
    (fileChecker) (println "Error: .git directory already exists\n")
    :else (folderMaker)))

;; init command
(defn init [args]
  (cond
    (= 1 (count args)) (gitInit)
    (or (= "-h" (second args)) (= "--help" (second args))) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    :else (println "Error: init accepts no arguments\n")
    ))

(defn -main [& args]
  (cond
    (= 0 (count args)) (println "nothing inputted change this later")
    (= "help" (first args)) (help (second args))
    (= "init" (first args)) (init args)
    ))