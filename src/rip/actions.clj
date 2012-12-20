(ns rip.actions
  "Some useful predefined actions based on webmachine."
  (:use rip.middleware
        ring.middleware.multipart-params
        ring.middleware.multipart-params.byte-array
        ring.middleware.multipart-params.temp-file
        rip.core))

(defn show
  "Creates a show action to be passed to resources.
   Options:
     -sufix
     -url-handler"
  [response entity-fetch-handler auth-handler allow-fn accepted-types
   get-etag & [route-opts]]
  (memb :show :get
        (-> response
            (wrap-etag get-etag)
            entity-fetch-handler
            (wrap-accept-header accepted-types)
            (wrap-allow allow-fn)
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "GET"})}
               route-opts)))

(defn index
  "Creates a index action to be passed to resources.
   Options:
     -sufix
     -url-handler"
  [response collection-fetch-handler auth-handler allow-fn accepted-types
   & [route-opts]]
  (coll :index :get
        (-> response
            collection-fetch-handler
            (wrap-accept-header accepted-types)
            (wrap-allow allow-fn)
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "GET"})}
               route-opts)))

(defn add
  "Creates an add action to be passed to resources.
   Options:
     -sufix
     -url-handler"
  [response entity-store-handler auth-handler allow-fn supported-types
   accepted-types & [route-opts]]
  (coll :add :post
        (-> response
            entity-store-handler
            (wrap-accept-header accepted-types)
            (wrap-supported-content-type supported-types)
            (wrap-allow allow-fn)
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "POST"})}
               route-opts)))

(defn update
  "Creates an update action to be passed to resources.
   Options:
     -sufix
     -url-handler"
  [response entity-store-handler auth-handler allow-fn supported-types
   accepted-types & [{:keys [title responses route-opts]}]]
  (memb :update :put
        (-> response
            entity-store-handler
            (wrap-accept-header accepted-types)
            (wrap-supported-content-type supported-types)
            (wrap-allow allow-fn)
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "PUT"
                                :title  (or title "update")})}
               route-opts)))

(defn destroy
  "Creates a destroy action to be passed to resources.
   Options:
     -sufix
     -url-handler"
  [response entity-remove-handler auth-handler allow-fn accepted-types
   & [route-opts]]
  (memb :destroy :delete
        (-> response
            entity-remove-handler
            (wrap-accept-header accepted-types)
            (wrap-allow allow-fn)
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "DELETE"})}
               route-opts)))

(defn upload
  "Creates a generic action for multipart uploading.

   Visit http://mmcgrana.github.com/ring/ring.middleware.multipart-params.html for more info.

   Parameter store-type can be either :temp or :byte-array.

   Options:
     -expires-in  value passed to temp-file-store if store-type is :temp
     -route-opts: route options(sufix, url-handler)"
  [response file-store-handler auth-handler allow-fn supported-types
   accepted-types store-type & [{:keys [route-opts expires-in]}]]
  (coll :upload :post
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
            auth-handler)
        (merge {:url-handler (fn [url]
                               {:href   url
                                :method "POST"})}
               route-opts)))
