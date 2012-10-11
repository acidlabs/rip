(ns rip.actions
  "Some useful predefined actions based on webmachine, to be used with rip.core/defaction."
  (:use rip.middleware
        ring.middleware.multipart-params
        ring.middleware.multipart-params.byte-array
        rip.core))

(defn show
  "Creates a show action to be passed to resources."
  [response entity-fetch-handler auth-handler allow-fn accepted-types get-etag
   & [{:keys [title responses]}]]
  (memb :show :get
        (binding [*responses* (merge *responses* responses)]
          (-> response
              (wrap-etag get-etag)
              entity-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        {:url-handler (fn [url]
                        {:show (merge {:href url} (when title {:title title}))})}))

(defn index
  "Creates a index action to be passed to resources."
  [response collection-fetch-handler auth-handler allow-fn accepted-types
   & [{:keys [title responses]}]]
  (coll :index :get
        (binding [*responses* (merge *responses* responses)]
          (-> response
              collection-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        {:url-handler (fn [url]
                        {:index (merge {:href url} (when title {:title title}))})}))

(defn add
  "Creates an add action to be passed to resources."
  [response entity-store-handler auth-handler allow-fn supported-types accepted-types
   & [{:keys [title responses]}]]
  (coll :add :post
        (binding [*responses* (merge *responses* responses)]
          (-> response
              entity-store-handler
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        {:url-handler (fn [url]
                        {:add (merge {:href url} (when title {:title title}))})}))

(defn upload
  "Creates a generic action for multipart uploading.
   Visit http://mmcgrana.github.com/ring/ring.middleware.multipart-params.html
   and for more info."
  [response file-store-handler auth-handler allow-fn supported-types accepted-types
   & [{:keys [title responses]}]]
  (coll :upload :post
        (binding [*responses* (merge *responses* responses)]
          (-> response
              file-store-handler
              (wrap-multipart-params {:store (byte-array-store)})
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        {:url-handler (fn [url]
                        {:upload (merge {:href url} (when title {:title title}))})}))
