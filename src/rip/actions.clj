(ns rip.actions
  "Some useful predefined actions based on webmachine, to be used with rip.core/defaction."
  (:use rip.middleware
        ring.middleware.multipart-params
        ring.middleware.multipart-params.byte-array
        ring.middleware.multipart-params.temp-file
        rip.core))

(defn show
  "Creates a show action to be passed to resources.
   Options:
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response entity-fetch-handler auth-handler allow-fn accepted-types get-etag
   & [{:keys [title responses route-opts]}]]
  (memb :show :get
        (binding [*responses* (merge *responses* responses)]
          (-> response
              (wrap-etag get-etag)
              entity-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:show (merge {:href url} (when title {:title title}))})}
               route-opts)))

(defn index
  "Creates a index action to be passed to resources.
   Options:
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response collection-fetch-handler auth-handler allow-fn accepted-types
   & [{:keys [title responses route-opts]}]]
  (coll :index :get
        (binding [*responses* (merge *responses* responses)]
          (-> response
              collection-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:index (merge {:href url} (when title {:title title}))})}
               route-opts)))

(defn add
  "Creates an add action to be passed to resources.
   Options:
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response entity-store-handler auth-handler allow-fn supported-types accepted-types
   & [{:keys [title responses route-opts]}]]
  (coll :add :post
        (binding [*responses* (merge *responses* responses)]
          (-> response
              entity-store-handler
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:add (merge {:href url} (when title {:title title}))})}
               route-opts)))

(defn update
  "Creates an update action to be passed to resources.
   Options:
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response entity-fetch-handler entity-store-handler auth-handler allow-fn supported-types
   accepted-types get-etag & [{:keys [title responses route-opts]}]]
  (coll :update :put
        (binding [*responses* (merge *responses* responses)]
          (-> response
              entity-store-handler
              (wrap-etag get-etag)
              entity-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:update (merge {:href url} (when title {:title title}))})}
               route-opts)))

(defn destroy
  "Creates a destroy action to be passed to resources.
   Options:
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response entity-fetch-handler entity-remove-handler auth-handler allow-fn accepted-types
   get-etag & [{:keys [title responses route-opts]}]]
  (memb :destroy :delete
        (binding [*responses* (merge *responses* responses)]
          (-> response
              entity-remove-handler
              (wrap-etag get-etag)
              entity-fetch-handler
              (wrap-accept-header accepted-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:destroy (merge {:href url} (when title {:title title}))})}
               route-opts)))

(defn upload
  "Creates a generic action for multipart uploading.

   Visit http://mmcgrana.github.com/ring/ring.middleware.multipart-params.html for more info.

   Parameter store-type can be either :temp or :byte-array.

   Options:
     -expires-in  value passed to temp-file-store if store-type is :temp
     -title:      title of the link
     -responses:  default middleware responses
     -route-opts: route options(sufix, url-handler)"
  [response file-store-handler auth-handler allow-fn supported-types accepted-types store-type
   & [{:keys [title responses route-opts expires-in]}]]
  (coll :upload :post
        (binding [*responses* (merge *responses* responses)]
          (-> response
              file-store-handler
              (wrap-multipart-params {:store (case store-type
                                               :temp (if expires-in
                                                       (temp-file-store expires-in)
                                                       (temp-file-store))
                                               :byte-array (byte-array-store))})
              (wrap-accept-header accepted-types)
              (wrap-supported-content-type supported-types)
              (wrap-allow allow-fn)
              auth-handler))
        (merge {:url-handler (fn [url]
                               {:upload (merge {:href url} (when title {:title title}))})}
               route-opts)))
