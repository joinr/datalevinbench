"USAGE: ./bench [rebuild]? [<version>|<version-vm> ...]? [<bench-name> ...]?"
(ns bench
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.shell :as sh]
    [clojure.string :as str]))

(defn parse-opts [args]
  (loop [opts {:rebuild    false
               :versions   []
               :benchmarks []}
         args args]
    (if-some [arg (first args)]
      (cond
        (= "rebuild" arg)
        (recur (assoc opts :rebuild true) (next args))

        (re-matches #"(datalevin|datascript|datomic)" arg)
        (recur (update opts :versions conj ["latest" arg]) (next args))

        (re-matches #"(\d+\.\d+\.\d+|[0-9a-fA-F]{40}|latest)" arg)
        (recur (update opts :versions conj [arg "datalevin"]) (next args))

        (re-matches #"(\d+\.\d+\.\d+|[0-9a-fA-F]{40}|latest)-(datalevin|datascript|datomic)" arg)
        (let [[_ version vm] (re-matches #"(\d+\.\d+\.\d+|[0-9a-fA-F]{40}|latest)-(datalevin|datascript|datomic)" arg)]
          (recur (update opts :versions conj [version vm]) (next args)))

        :else
        (recur (update opts :benchmarks conj arg) (next args)))
      opts)))

(def default-benchmarks
  [;"add-1"
   ;"add-5"
   ;"add-all"
   ;"init"
   ;"retract-5"
   "q1"
   "q2"
   "q3"
   "q4"
   ;; "q5"
   "qpred1"
   "qpred2"])

(def default-opts
  {:benchmarks default-benchmarks
   :versions   [["latest" "datascript"]
                ["latest" "datalevin"]
                ["latest" "datahike-mem"]
                ["latest" "datahike-file"]]})

(defn run-benchmarks [version vm benchmarks]
  (case vm
    "datalevin"     (apply (requiring-resolve 'datalevin-bench.datalevin/-main) benchmarks)
    "datascript"    (apply (requiring-resolve 'datascript-bench.datascript/-main) benchmarks)
    "datahike-mem"  (apply (requiring-resolve 'datahike-bench.datahike/-main) :mem benchmarks)
    "datahike-file" (apply (requiring-resolve 'datahike-bench.datahike/-main) :file benchmarks)))

(defn run-all [opts]
  (let [{:keys [rebuild benchmarks versions]} opts
        _ (println benchmarks versions)]
    (print "version   \t")
    (doseq [b benchmarks] (print b "\t"))
    (println)
    (doseq [[version vm] versions]
      (print (str version "-" vm) "\t")
      (flush)
      (run-benchmarks version vm benchmarks))))

(defn -main [& args]
  (let [opts (if (seq args)
               (parse-opts args)
               default-opts)
        _    (println [:running-with opts])]
    (run-all opts)
    (shutdown-agents)
    (System/exit 0)))

; (System/exit 0)
