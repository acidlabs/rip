(ns rip.db
  (:import rip.RipException)
  (:use korma.core
        korma.db))

(def transaction-exception (RipException. {:code :transaction-exception :message "Transaction error"}))

(defn wrap-transaction
  "Excecutes handler inside a transaction and rollbacks if any exception is rised."
  [handler]
  (fn [request]
    (transaction
      (try
        (handler request)
        (catch Exception e
          (rollback)
          (throw transaction-exception))))))

(defn page
  "Fetches a page from the given entity"
  [ent page page-size order-fields [where-clause joins]]
  (select
      (reduce
       (fn [query [ent clause]] (join query ent clause))
       (reduce
        (fn [query [field ord]] (apply order query field ord))
        (-> (select* ent)
            (where where-clause)
            (limit page)
            (offset (* page page-size)))
        order-fields)
       joins)))

(defn- get-fk
  {:no-doc true}
  [ent1 ent2]
  (keyword (last (clojure.string/split (val (first @((:rel ent1) (:name ent2)))) #"\""))))

(defn insert-with-refs
  "Inserts a single value with outer and inner references."
  [ent value outer-refs & [inner-refs]]
  (let [new-value* (apply dissoc value (concat (keys outer-refs) (keys inner-refs)))
        new-value (if inner-refs
                    (reduce
                     (fn [value [k v]]
                       (assoc value k v))
                     new-value*
                     (map (fn [[k ref-ent]]
                            (let [pk ((:pk ref-ent) (insert ref-ent (values (k value))))]
                              [(get-fk ent ref-ent) pk]))
                          inner-refs))
                    new-value*)
        new-pk ((:pk ent) (insert ent (values new-value)))]
    (doseq [[k ref-ent] outer-refs]
      (let [fk (get-fk ent ref-ent)]
        (insert ref-ent
                (values (map (fn [val] (assoc val fk new-pk)) (value k))))))
    new-pk))
