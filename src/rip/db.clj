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
