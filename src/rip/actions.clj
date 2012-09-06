(ns rip.actions
  "Some useful predefined actions based on webmachine, to be used with rip.core/defaction."
  (:use rip.middleware
        rip.core))

(defn show
  "Creates a show action to be passed to resources."
  [entity-handler entity-fetch-handler auth-handler allow-fn accepted-types get-etag
   & [{:keys [title responses]}]]
  (memb :show :get
        (binding [*responses* (merge *responses* responses)]
          (-> entity-handler
              (wrap-etag get-etag)
              entity-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        (fn [url]
          {:show (merge {:href url} (when title {:title title}))})))

(defn index
  "Creates a index action to be passed to resources."
  [collection-handler collection-fetch-handler auth-handler allow-fn accepted-types
   & [{:keys [title responses]}]]
  (coll :index :get
        (binding [*responses* (merge *responses* responses)]
          (-> collection-handler
              collection-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        (fn [url]
          {:index (merge {:href url} (when title {:title title}))})))

(defn add
  "Creates an add action to be passed to resources."
  [entity-handler entity-store-handler auth-handler allow-fn supported-types accepted-types
   & [{:keys [title responses]}]]
  (coll :add :post
        (binding [*responses* (merge *responses* responses)]
          (-> entity-handler
              entity-store-handler
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        (fn [url]
          {:add (merge {:href url} (when title {:title title}))})))
