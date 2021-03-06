(ns rip.util
  (:use rip.core))

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

(defn in?
  [& actions]
  (or (empty? actions)
      (contains? (set actions) *current-action*)))
