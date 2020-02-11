(ns idiot)

;; help command
(defn help [args]
  (cond
    (or (= "-h" args) (= "--help" args)) (println "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>")
    (= "init" args) (println "Usage: idiot init")
    (= "help" args) (println "Usage: idiot help <command>")
    (= "hash-object" args) (println "Usage: idiot hash-object [-w] <file>")
    :else (println "Error: invalid command\n")))

(defn -main [& args]
  (cond
    (= "help" (first args)) (help (second args))
    ))