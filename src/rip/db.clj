(ns rip.db
  (:use
   clojureql.core
   clojureql.predicates)
  (:refer-clojure
   :exclude [spit extend compile distinct drop group-by take sort conj! disj! case]))

;; Standardize on usage of record-crap within and amongst functions

(def *cljql-identity* (=* 1 1))

(declare search-by-refs select-by-key select-by-fuzzy-example
         select-by-exact-example count-query cut-by-paging apply-sorting make-joins)

(defn file-atts
  [record-map]
  (reduce merge
          {}
          (filter
           (fn [[att-ref value]]
             (and
              (is-type? att-ref Attribute)
              (= (:data-type att-ref) org.zkoss.util.media.Media)
              value))
           record-map)))

(defn save-file
  [media file-att entity-type]
  (let [path (str *files-path*
                  (:single-name entity-type) "/" (:att-name file-att) "/"
                  (milli now) (.getName media))]
    (with-open [f (file path)]
      (make-parents f)
      (copy (.getStreamData media) f))
    (with-meta media {:path path})))

(defn remove-file
  [path]
  (with-open [f (file path)]
    (delete-file f)))

(defn save-files
  [record-maps]
  (reduce
   (fn [record-maps r-map]
     (conj record-maps
           (reduce
            (fn [record-map [file-att media]]
              (recordmap/assoc-val record-map file-att (save-file media file-att (:entity-type record-map))))
            r-map
            (file-atts r-map))))
   []
   record-maps))

(defn delete-files
  [record-maps]
  (doseq [r-map record-maps]
    (doseq [[file-att media] (file-atts r-map)]
      (remove-file (:path (meta media))))))

(defn create
  [record-map & record-maps]
  (let [records (save-files (cons record-map record-maps))]
    (try (conj! (recordmap/record-map->table record-map) (map #(recordmap/record-map->hack-map %) records))
         (catch java.sql.SQLException e
           (do
             (delete-files records)
             (.getErrorCode e))))))

(defn update
  ([old-map new-map]
     (update {old-map new-map}))
  ([old-new-map]
     (let [persist-table (recordmap/record-map->table (val (first old-new-map)))]
       (try
         (doseq [[old-map new-map] old-new-map]
           (update-in!
            persist-table
            (where (reduce and* (for [[k v] (recordmap/record-map->hack-map old-map)] (=* k v))))
            new-map))
         (catch java.sql.SQLException e
           (.getErrorCode e))))))

(defn delete
  [record-map]
  (let [persist-table  (table db/db table-name)]
    (try @(disj!
           persist-table
           (where (reduce and* (for [[k v] (recordmap/record-map->hack-map record-map)] (=* k v)))))
         (catch java.sql.SQLException e
           (.getErrorCode e)))))

(defn search-all
  [entity-type]
  (map #(recordmap/hack-map->record-map entity-type %)
       @(table db/db (:table-name entity-type))))

(defn exists?
  [record-map]
  (not (nil? @(->
               (recordmap/record-map->table record-map)
               (select-by-exact-example
                (:table-name (:entity-type record-map))
                (recordmap/record-map->hack-map
                 {(:entity-type record-map)
                  (recordmap/select-pks (first record-map))}))))))

(defn search-with-count
  [query entity-type]
  (vector (count-query query)
          (map #(recordmap/hack-map->record-map entity-type %) @query)))

(defn search-by-refs
  [query record-map]
  (-> (apply
       select-by-fuzzy-example
       (cons query (recordmap/record-map->hack-map record-map)))
      (make-joins record-map)
      (project [:*])))

(defn search-by-key
  [query record-map word]
  (select-by-key
   query
   (:table-name (:entity-type record-map))
   word
   (map #(:col-name %)
        (filter #(= (:data-type %) String) (:atts (:entity-type record-map))))))


(defn search-with-criteria
  ([page per-page sort-field sort-order]
     (fn [query]
       (-> (cut-by-paging query page per-page)
           (#(if sort-field
               (apply-sorting % sort-field sort-order)
               %)))))

  ;; criteria
  ;; criteria + refs
  ([example page per-page sort-field sort-order]
     (let [criteria-query (-> (recordmap/record-map->table example)
                              ((search-with-criteria page per-page sort-field sort-order)))
           entity-type (:entity-type example)]
       (if (empty? (recordmap/children example))
         (search-with-count criteria-query entity-type)
         (search-with-count (search-by-refs criteria-query example) entity-type))))

  ;; criteria + key
  ;; criteria + key + refs
  ([example word page per-page sort-field sort-order]
     (let [key-query (-> (recordmap/record-map->table example)
                         ((search-with-criteria page per-page sort-field sort-order))
                         (search-by-key example word))
           entity-type (:entity-type example)]
       (if (empty? (recordmap/children example))
         (search-with-count key-query entity-type)
         (search-with-count (search-by-refs key-query example) entity-type)))))

(defn count-query
  [query]
  (second (ffirst @(aggregate query [:count/*]))))

(defn cut-by-paging
  [query page per-page]
  (-> (drop query (int (* page per-page)))
      (limit per-page)))

(defn apply-sorting
  [query sort-field sort-order]
  (sort query [(keyword (str (name sort-field) sort-order))]))

(defrecord Alias-tree
    [#^String prefix entity-type children])

(defn make-alias-tree
  [prefix #^Record-map node]

  (Alias-tree.
   prefix
   (:entity-type node)
   (reduce merge
           {}
           (map
            (fn [entry]
              (hash-map
               (recordmap/tag entry)
               (make-alias-tree
                (str prefix "_" (name (:key-name (recordmap/tag entry))))
                (val entry))))
            (filter recordmap/inner? (:children node)))

           )))

(defn at-has-children?
  [alias-tree]
  (not-empty (:children alias-tree)))

(defn make-table
  "Constructs a table object from an aliases-tree node"
  [alias-tree]
  (table (:table-name (:entity-type alias-tree))
         (:prefix alias-tree)))

(defn make-joins
  "Constructs ..."
  ([query #^recordmap.Record-map example]

     (defn- high-filter
       ([exprs val]
          (high-filter exprs val val))

       ([exprs val default]
          (cond
            (or (map? exprs) (not (coll? exprs)))
            (if (not= exprs val)
              exprs
              default)
            :else
            (let [filtered (filter #(not= % val) exprs)]
              (if (not-empty filtered)
                filtered
                default))
            )))

     (defn- make-alias
       "Constructs the aliased form keyword of a column corresponding to a table in the joins-tree"
       [node column]
       (let [prefix (if (string? node) ;; the base case
                      node
                      (:prefix node))]
         (str (name prefix) "." (name column))))

     (defn- join-columns
       "Creates the equality predicates to join columns between two tables along a branch of the joins-tree"
       [source-node reference]

       (high-filter (reduce and*
                            (for [[local foreign] (:fks-pks ((:rel reference)))]
                              (=*
                               (make-alias source-node local)
                               (make-alias (get (:children source-node) reference) foreign))))
                    (and*)
                    *cljql-identity*))

     (defn- compare-value
       "Constructs a comparison predicate for a certain column and a value"
       [op #^Alias-tree destination-node attribute value]

       (op (make-alias destination-node
                       (:col-name attribute))
           value))

     (defn- join-values
       "Constructs comparison predicates to restrict the concrete values of a certain join expression"
       ([destination-node example-node]
          (->
           (reduce
            and*
            *cljql-identity*
            (high-filter
             (map
              (fn [[tag value]]
                (if (is-type? tag Interval)
                  (and*
                   (compare-value >=* destination-node (:att tag) (:from value))
                   (compare-value <=* destination-node (:att tag) (:to value)))
                  (compare-value =* destination-node tag value)))
              (filter recordmap/leaf? (recordmap/children example-node)))
             *cljql-identity*))

           (high-filter

            (and* *cljql-identity* *cljql-identity*)
            *cljql-identity*))))

     (defn- join-on
       "Generates the full join between two nodes of the tree"
       [query source-alias reference example-branch]
       (if-let [destination-alias (get (:children source-alias)
                                       reference)]
         (join
          query
          (make-table destination-alias)
          (where (and
                  (join-columns source-alias reference)
                  (join-values destination-alias example-branch))))
         query))

     (letfn [(join-children
               [query source-alias example-record]
               (reduce
                (fn [acc-query child-node]
                  (join-branch
                   acc-query
                   source-alias
                   (recordmap/tag child-node)
                   (val child-node)))
                query
                (filter recordmap/inner? (recordmap/children example-record))))

             (join-branch
               [query source-alias reference example-branch]
               (join-children
                (join-on query source-alias reference example-branch)
                (get (:children source-alias) reference)
                example-branch))]

       (join-children
        query
        (make-alias-tree "" example)
        example))))

(defn select-by-key
  [query table word atts]
  (select query
          (where
           (apply or*
                  (map (fn [x] (like (keyword (str (name table) "." (name x))) word)) atts)))))

(defn select-by-fuzzy-example
  ([query table from]
     (select-by-fuzzy-example query table from nil))
  ([query table from to]
     (select query
             (where
              (apply and*
                     (for [[k v] from]
                       (cond
                         (string? v)
                         (like (keyword (str (name table) "." (name k))) v)

                         (map? v)
                         (apply and*
                                (if (nil? (k to))
                                  (for [[func value] v]
                                    (let [app-func (keyword (str func "/" (name (keyword (str (name table) "." (name k))))))]
                                      (=* app-func value)))
                                  (for [[func value] v]
                                    (let [app-func (keyword (str func "/" (name (keyword (str (name table) "." (name k))))))
                                          to-func ((k to) func)]
                                      (if (nil? to-func)
                                        (=* app-func value)
                                        (and* (>=* app-func value) (<=* app-func to-func)))))))

                         :else
                         (if (nil? (k to))
                           (=* (keyword (str (name table) "." (name k))) v)
                           (and* (>=* (keyword (str (name table) "." (name k))) v) (<=* (keyword (str (name table) "." (name k))) (k to)))))))))))

;; hack-map -> query
(defn select-by-exact-example
  [query table example]
  (select query
          (where
           (reduce and*
                   (for [[k v] example] (=* (keyword (str (name table) "." (name k))) v))))))
