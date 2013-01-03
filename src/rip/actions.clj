(ns rip.actions
  "Some useful predefined actions based on webmachine."
  (:use rip.middleware
        ring.middleware.multipart-params
        ring.middleware.multipart-params.byte-array
        ring.middleware.multipart-params.temp-file
        rip.core))

(def methods-map
  {:get    "GET"
   :post   "POST"
   :put    "PUT"
   :delete "DELETE"})

(defn action-link
  [method]
  {:url-handler (fn [url]
                  {:href   url
                   :method (method methods-map)})})

(wrap-accept-header accepted-types)
(wrap-supported-content-type supported-types)
(wrap-allow allow-fn)
auth-handler

wrap-etag-header
wrap-if-modified-since
wrap-if-none-match
wrap-if-unmodified-since
wrap-if-match
wrap-last-modified

(defn show
  "Creates a show action to be passed to resources.
   The find-resource function should return a map representing the resource
   with optional keys etag and last-modified.
   Options map:
     allow-fn:     Checks permissions on this action
     access-fn:    Checks permissions on the resource
     accept-types: Set of matching types from the accept header"
  [find-resource show-handler &
   [{:keys [allow-fn access-fn accepted-types]}]]
  (memb :show
        :get
        (-> entity-response
            show-handler

            (wrap-access access-fn)
            find-resource
            (wrap-allow allow-fn))
        (action-link :get)))

(defn index
  "Creates a index action to be passed to resources."
  [index-handler & [{:keys [allow-fn access-fn accepted-types]}]]
  (coll :index
        :get
        (-> entity-response
            index-handler
            (wrap-allow allow-fn))))

(defn add
  "Creates an add action to be passed to resources."
  [add-handler location-handler
   & [{:keys [allow-fn access-fn validator accepted-types supported-types]}]]
  (coll :add
        :post
        (-> created-response
            (wrap-location-header location-handler)
            add-handler
            (wrap-access access-fn)
            (wrap-validator validator)
            (wrap-allow allow-fn))
        (action-link :put)))

(defn update
  "Creates an update action to be passed to resources."
  [handler etag-fn & [allow-fn]]
  (memb :update
        :put
        (-> entity-response
            show-handler

            (wrap-access access-fn)
            find-resource
            (wrap-allow allow-fn))
        (action-link :put)))

(defn destroy
  "Creates a destroy action to be passed to resources."
  [handler & [allow-fn]]
  (action memb :destroy :delete handler entity-response allow-fn))

(defn upload
  "Creates a generic action for multipart uploading.

   Visit http://mmcgrana.github.com/ring/ring.middleware.multipart-params.html
   for more info.

   Parameter store-type can be either :temp or :byte-array.

   Options map:
     allow-fn:    function to check permissions to the resource
     expires-in:  value passed to temp-file-store if store-type is :temp"
  [handler location-handler store-type & [{:keys [allow-fn expires-in]}]]
  (coll :upload
        :post
        (wrap handler
              (wrap-multipart-params
               {:store (case store-type
                         :temp (if expires-in
                                 (temp-file-store expires-in)
                                 (temp-file-store))
                         :byte-array (byte-array-store))}))
        entity-response
        allow-fn))
