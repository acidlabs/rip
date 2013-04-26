(ns rip.util
  (:use rip.core))

(defn hateoas
  [value links]
  (cond
   (map? value)
   (assoc value
     :links
     (reduce
      (fn [links [rel link]]
        (assoc links rel (apply path-for link)))
      {}
      links))
   (sequential? value)
   (map (fn [v] (hateoas links)) value)))

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
  [request map]
  (assoc request
    :params
    (reduce
     (fn [params [k v]]
       (assoc params (keyword (str "_" (name k))) v))
     (:params request)
     map)))
