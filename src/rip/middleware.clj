(ns rip.middleware
  "Wrappers to be used for rip actions or ring middleware.
   Some wrappers recives an optional response to be merged with the default error response."
  (:require [cheshire.core :as json]
            [clojure.data.xml :as xml])
  (:use com.twinql.clojure.conneg))

(def ^{:dynamic true :doc "Default xml serialization tags"}
  *xml-tags* {:list :list :item :item})

(def ^{:dynamic true :doc "Default error responses"} *responses*
  {:forbidden               {:status 403 :body "Forbidden."}
   :unsupported-media-tpye  {:status 415 :body "Unsupported Media Type"}
   :request-entity-too-long {:status 413 :body "Request entity too large."}
   :not-acceptable          {:status 406 :body "Not Acceptable"}
   :precondition-failed     {:status 412 :body "Precondition Failed."}
   :unauthorized            {:status 401 :body "Unauthorized."}
   :not-modified            {:status 304 :body "Not Modified."}})

(defn assoc-input [request input]
  (assoc-in request [:context :input] input))

(defn get-input [request]
  (get-in request [:context :input]))

(defn assoc-output [request output]
  (assoc-in request [:context :output] output))

(defn get-output [request]
  (assoc-in request [:context :output]))

(defn- xml->hash-map
  "Transforms clojure.data.xml.Element to clojure maps.
   To keep an analog form to json transformations, a 'list' tag must be specified
   (default to :list in *xml-tags* global varible)."
  [{:keys [tag content]}]
  (if (= tag (*xml-tags* :list))
    (reduce (fn [m e]
              (let [[tag content] (first (xml->hash-map e))]
                (conj m content)))
            []
            content)
    {tag (if (= (type (first content)) clojure.data.xml.Element)
           (let [first-content (xml->hash-map (first content))]
             (if (vector? first-content)
               first-content
               (reduce (fn [m e]
                         (let [[tag content] (first (xml->hash-map e))]
                           (assoc m tag content)))
                       {}
                       content)))
           (first content))}))

(defn- map->xml
  [tag cont]
  (apply
   xml/element
   (concat [tag nil]
           (if (coll? cont)
             (if (map? cont)
               (map #(apply map->xml %) cont)
               [(apply xml/element
                       (concat [(*xml-tags* :list) nil]
                               (map (fn [val]
                                      (map->xml (*xml-tags* :item) val))
                                    cont)))])
             [(str cont)]))))

(defn gen-xml [value tag]
  (xml/emit-str (map->xml tag value)))

(defn parse-xml [s]
  (val (first (xml->hash-map (xml/parse-str s)))))

(defn entity-response
  [status]
  (fn [request]
    (let [content-type
          (or (get-in request [:context :accept-content-type])
              (get-in request [:headers "content-type"])
              "application/json")
          entity (get-output request)]
      {:status  status
       :body    (case content-type
                  "application/xml"
                  (gen-xml entity (get-in request [:context :xml-tag]))
                  (json/generate-string entity))
       :headers {"content-type" (if (contains? #{"*/*" "application/*"}
                                               content-type)
                                  "application/json"
                                  content-type)}})))

(def ok-entity-response (make-entity-response 200))

(def created-response (make-entity-response 201))

(defn- get-cause [e]
  (if-let [cause (.getCause e)]
    (get-cause cause)
    e))

(defn wrap-server-error
  "Wrap a handler such that exceptions are handled with the given error-function."
  [handler error-function]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (error-function (get-cause ex))))))

(defn wrap-allow
  "Used to check for permissions on resources."
  [handler allow-fn & [response]]
  (fn [request]
    (if (allow-fn request)
      (handler request)
      (*responses* :forbidden))))

(defn wrap-supported-content-type
  "Validates the content type from the request."
  [handler content-types & [response]]
  (fn [request]
    (if-let [c-t (get-in request [:headers "content-type"])]
      (if (best-allowed-content-type  content-types)
        (*responses* :unsupported-media-tpye))
      (handler request))))

(defn wrap-request-entity-length
  "Validates the length of the request body."
  [handler body-max-length & [response]]
  (fn [request]
    (if (> (count (slurp (:body request))) body-max-length)
      (handler request)
      (*responses* :request-entity-too-long))))

(defn wrap-body-parser
  "Wrap the body to a clojure map, only for json and xml inputs.
   The result map is stored as :input in the :context map of the request."
  [handler xml-tags]
  (fn [request]
    (let [bstr (slurp (:body request))
          input (case (second (best-allowed-content-type
                               (get-in request [:headers "content-type"]) #{"application/*"}))
                  "json" (json/parse-string bstr true)
                  "xml"  (binding [*xml-tags* (merge xml-tags *xml-tags*)]
                           (parse-xml bstr))
                  :else bstr)]
      (handler (assoc-input request input)))))

(defn wrap-accept-header
  "Checks the Accept header and validates based on the given supported content types.
   If the the content type is supported then the best type from the content negotiation is
   stored as :accept-content-type in the :context map of the request."
  [handler content-types & [default-type]]
  (fn [{headers :headers :as request}]
    (if-let [accept (headers "accept")]
      (if-let [[app format] (not-empty (best-allowed-content-type accept content-types))]
        (handler (assoc-in request [:context :accept-content-type] (str app "/" format)))
        (*responses* :not-acceptable))
      (handler request))))

(defn wrap-if-match
  "Compares the etag from the result of calling the given function."
  [handler get-etag]
  (fn [request]
    (let [etag (get-in request [:headers "if-match"])]
      (if (and etag (or (= etag "*") (= etag (get-etag request))))
        (handler request)
        (*responses* :precondition-failed)))))

(defn wrap-auth-header
  "Cecks the Authorization header."
  [handler & [response]]
  (fn [request]
    (if ((:headers request) "authorization")
      (handler request)
      (*responses* :unauthorized))))

(defn wrap-default-responses
  "Sets the default responses for middlewares in this namespace"
  [handler responses]
  (binding [*responses* (merge responses *responses*)]
    (fn [request]
      (handler request))))

(defn wrap-location-header
  "Sets the location header from passing the request to the given function.
   The function should return the url of the created resource"
  [handler get-url]
  (fn [request]
    (assoc-in (handler request) [:headers "location"] (get-url request))))

(defn wrap-body-validation
  [handler validator]
  (fn [request]
    (let [{:keys [valid? input output]} (validator (get-input request))]
      (if valid?
        (handler (assoc-input request output))
        (entity-response input 400 request)))))
