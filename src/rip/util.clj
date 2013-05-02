(ns rip.util
  (:use rip.core
        [korma.core :exclude (nest create)]))

(defn wrap-fn
  [handler f]
  (fn [request] (f (handler request))))

(defmacro wrap-macro
  [handler macro & args]
  `(fn [request#] (~macro (~handler request#) ~@args)))

(defn get-action
  [res [action & actions]]
  (if-let [action (get-in res [:actions action])]
    (if actions
      (throw (Exception. "Path not found"))
      action)
    (let [res (get-in res [:nested action])]
      (if (and res actions)
        (get-action res actions)
        (throw (Exception. "Path not found"))))))

(defn assoc-links
  [value paths]
  (cond
   (map? value)
   (reduce
    (fn [value [rel [res path :as link]]]
      (let [{:keys [method]} (get-action res path)]
        (assoc-in value
                  [:links rel]
                  {:href (apply path-for link)
                   :method (clojure.string/upper-case
                            (name method))})))
    value
    paths)
   (sequential? value)
   (map (fn [v] (assoc-links v paths)) value)))

(defn create-response
  [key value location & [links]]
  {:status 201
   :body {key (if links (assoc-links value links) value)}
   :headers {"Location" location}})

(defmacro upload
  [res opts bindings & body]
  `(action
    ~res
    [:upload ""]
    :post
    :collection
    (-> (fn [request#]
          (let-request [~bindings request#] ~@body))
        (wrap-multipart-params ~opts))))

(defn assoc-globals
  [request globals]
  (reduce
   (fn [request [k v]]
     (assoc-in
      request
      [:params (keyword (str "*" (name k) "*"))]
      v))
   request
   globals))

(defn wrap-keys
  [res collection member]
  (-> res
      (wrap [:index :show :edit :destroy] (wrap-fn (fn [b] {:body b})))
      (wrap [:show :edit :destroy] (wrap-fn (fn [u] {member u})))
      (wrap [:index] (wrap-fn (fn [u] {collection u})))))

;; (defn wrap-collection
;;   [handler name path & [{:keys [page-size item-fn]}]]
;;   (h [page per_page :as req]
;;      (let [query     (handler req)
;;            total     (aggregate query )
;;            page (or page 1)
;;            page-size (or per_page page-size)]
;;        (assoc-links
;;         {name (map
;;                item-fn
;;                (select query
;;                        (limit page-size)
;;                        (offset (* page-size (dec page)))))}
;;         {:prev
;;          :next
;;          :first
;;          :last}))))
