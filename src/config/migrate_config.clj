(ns config.migrate-config)

(defn migrate-config []
  {:directory "/test/migrations"
   :current-version (fn [])
   :update-version (fn [])})
