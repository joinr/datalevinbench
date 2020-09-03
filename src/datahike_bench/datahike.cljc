(ns datahike-bench.datahike
  (:require
    [clojure.string :as str]
    [datahike.api :as d]
    [datascript-bench.core :as core] ;;shared
    ))

;; test-db
(defn get-config [name backend]
  (case backend
    :mem {:store          {:backend :mem :id name}
          :schema-flexibility :write
          :keep-history? true
          :index          :datahike.index/hitchhiker-tree}

    :file {:store          {:backend :file :path (str "./" name)}
           :schema-flexibility :write
           :keep-history? true
           :index          :datahike.index/hitchhiker-tree}
    (throw (ex-info "unknown datahike backend!"
                    {:name name :backend backend}))))

(def ^:dynamic *backend* :mem)

(defn- schema-attr [name type & {:as args}]
  (merge
    {:db/id          (d/tempid :db.part/db)
     :db/ident       name
     :db/valueType   type
     :db/cardinality :db.cardinality/one
     ;:db.install/_attribute :db.part/db
     }
    args))

(defn new-conn
  ([] (new-conn "bench"))
  ([name] (new-conn name  *backend*))
  ([name backend]
   (let [cfg (get-config name backend)]
      (d/delete-database cfg)
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (d/transact conn
          [ (schema-attr :name      :db.type/string)
            (schema-attr :last-name :db.type/string)
            (schema-attr :sex       :db.type/keyword)
            (schema-attr :age       :db.type/long)
            (schema-attr :salary    :db.type/long)
            (schema-attr :follows   :db.type/ref, :db/cardinality :db.cardinality/many)])
        conn))))


(defn db-with [conn tx-data]
  (-> conn
      (d/transact tx-data)
      :db-after))


(def db100k
  (db-with (new-conn "db100k") core/people20k))


(defn tempid [i]
  (d/tempid :db.part/user (- i)))


(defn- wide-db
  ([depth width] (db-with (new-conn) (wide-db 1 depth width)))
  ([id depth width]
    (if (pos? depth)
      (let [children (map #(+ (* id width) %) (range width))]
        (concat
          (map #(array-map
                 :db/id   (tempid id)
                 :name    "Ivan"
                 :follows (tempid %)) children)
          (mapcat #(wide-db % (dec depth) width) children)))
      [{:db/id (tempid id) :name "Ivan"}])))

(defn- long-db [depth width]
  (db-with
    (new-conn)
    (apply concat
      (for [x (range width)
            y (range depth)
            :let [from (+ (* x (inc depth)) y)
                  to   (+ (* x (inc depth)) y 1)]]
        [{:db/id   (tempid from)
          :name    "Ivan"
          :follows (tempid to)}
         {:db/id   (tempid to)
          :name    "Ivan"}]))))

;;tests

(defn ^:export add-1 []
  (core/bench
    (let [conn (new-conn)]
      (doseq [p core/people20k]
        (let [report(d/transact conn [[:db/add "p" :name (:name p)]])
              id     (get (:tempids report) "p")]
          (d/transact conn [[:db/add id :last-name (:last-name p)]])
          (d/transact conn [[:db/add id :sex       (:sex p)]])
          (d/transact conn [[:db/add id :age       (:age p)]])
          (d/transact conn [[:db/add id :salary    (:salary p)]]))))))


(defn ^:export add-5 []
  (core/bench
    (let [conn (new-conn)]
      (doseq [p core/people20k]
       (d/transact conn [p])))))


(defn ^:export add-all []
  (core/bench
    (let [conn (new-conn)]
     (d/transact conn core/people20k))))


(defn ^:export retract-5 []
  (core/bench
    (let [conn (new-conn)
          db   (db-with conn core/people20k)
          eids (->> (d/datoms db :aevt :name) (map :e) (shuffle))]
      (doseq [eid eids]
       (d/transact conn [[:db.fn/retractEntity eid]])))))


(defn ^:export q1 []
  (core/bench
    (d/q '[:find ?e
           :where [?e :name "Ivan"]]
      db100k)))

(defn ^:export q2 []
  (core/bench
    (d/q '[:find ?e ?a
           :where [?e :name "Ivan"]
                  [?e :age ?a]]
      db100k)))

(defn ^:export q3 []
  (core/bench
    (d/q '[:find ?e ?a
           :where [?e :name "Ivan"]
                  [?e :age ?a]
                  [?e :sex :male]]
      db100k)))

(defn ^:export q4 []
  (core/bench
    (d/q '[:find ?e ?l ?a
           :where [?e :name "Ivan"]
                  [?e :last-name ?l]
                  [?e :age ?a]
                  [?e :sex :male]]
      db100k)))

(defn ^:export q5 []
  (core/bench
   (d/q '[:find ?e1 ?l ?a
          :where
          [?e :name "Ivan"]
          [?e :age ?a]
          [?e1 :age ?a]
          [?e1 :last-name ?l]]
        db100k)))

(defn ^:export qpred1 []
  (core/bench
    (d/q '[:find ?e ?s
           :where [?e :salary ?s]
                  [(> ?s 50000)]]
      db100k)))

(defn ^:export qpred2 []
  (core/bench
    (d/q '[:find ?e ?s
           :in   $ ?min_s
           :where [?e :salary ?s]
                  [(> ?s ?min_s)]]
      db100k 50000)))

(defn ^:export bench-rules []
  (doseq [[id db] [["wide 3×3" (wide-db 3 3)]
                   ["wide 5×3" (wide-db 5 3)]
                   ["wide 7×3" (wide-db 7 3)]
                   ["wide 4×6" (wide-db 4 6)]
                   ["long 10×3" (long-db 10 3)]
                   ["long 30×3" (long-db 30 3)]
                   ["long 30×5" (long-db 30 5)]]]
    (core/bench {:test "rules" :form id}
                (d/q '[:find ?e ?e2
                       :in   $ %
                       :where (follows ?e ?e2)]
                     db
                     '[[(follows ?x ?y)
                        [?x :follows ?y]]
                       [(follows ?x ?y)
                        [?x :follows ?t]
                        (follows ?t ?y)]]))))

(defn ^:export -main [backend & names]
  (let [backends {:mem    :mem
                  :file   :file
                  ":mem"  :mem
                  ":file" :file}]
    (binding [*backend* (or (backends backend)
                            (throw (ex-info "expected :mem or :file backend as first arg to -main!"
                                   {:backend backend})))]
    (doseq [n names]
      (if-some [benchmark (ns-resolve 'datahike-bench.datahike (symbol n))]
        (let [perf (benchmark)]
          (print (core/round perf) "\t")
          (flush))
        (do
          (print "---" "\t")
          (flush))))
    (println))))
