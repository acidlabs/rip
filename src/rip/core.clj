(ns rip.core
  "Provides a defaction macro for a more RESTFul definition of request handlers."
  (:use compojure.core
        hiccup.util)
  (:require [clojure.string :as string]
            [clout.core :exclude (Route)]))

(defrecord Route [method path route-handler url-handler])

(defrecord Member [method res-name path sufix route-handler url-handler])

(defrecord Collection [method res-name path sufix route-handler url-handler])

(defrecord Resources [res-name path id resources])

(defn- gen-resources
  [{:keys [res-name resources] :as res} & [parent-path]]
  (let [path        (if parent-path (str parent-path "/" (name res-name)) (str "/" (name res-name)))
        id          (if parent-path (str ":" (name res-name) "-id") ":id")
        member-path (str path "/" id)]
    (assoc res
      :path path
      :id id
      :resources
      (reduce
       (fn [resources resource]
         (assoc resources (:res-name resource)
                (condp #(= (type %2) %1) resource
                  Member     (assoc resource :path
                                    (str member-path (if-let [sufix (:sufix resource)]
                                                       (str "/" (name sufix)))))
                  Collection (assoc resource :path
                                    (str path (if-let [sufix (:sufix resource)]
                                                (str "/" (name sufix)))))
                  Resources  (gen-resources resource member-path)
                  (throw (Exception. "A resource must be either of type Member, Collection or Resources")))))
       {}
       (if (map? resources) (vals resources) resources)))))

(defn resources
  "Similar to rails resources, provides a way to define routes based on RESTful like actions.
   Receives a name and a series of actions defined through functions memb and/or coll.
   Usage:
          (resources :users
                     (memb :show :get (fn))
                     (coll :index :get identity))"
  [res-name & res]
  (gen-resources (Resources. res-name nil nil res)))

(defn memb
  "Creates a route of a member to be passed to a resources definition.
   Usage:
          (resources :users (memb :show :get identity))
           => /users/:id
   Options:
          - sufix
          - url-handler"
  [res-name method route-handler & [{:keys [sufix url-handler]}]]
  (Member. method res-name "" sufix route-handler (if url-handler url-handler identity)))

(defn coll
  "Creates a route to the collection of a resources definition.
   Usage:
          (resources :users (memb :show :get identity))
           => /users/:id
   Options:
          - sufix
          - url-handler"
  [res-name method route-handler & [{:keys [sufix url-handler]}]]
  (Collection. method res-name "" sufix route-handler (if url-handler url-handler identity)))

(defn route
  "Creates a route object"
  [method path route-handler & [url-handler]]
  (Route. method path route-handler (if url-handler url-handler identity)))

(defmulti ->handler
  "Generates the ring handler function for the passed resource"
  type)

(defmethod ->handler Resources
  [{:keys [resources]}]
  (apply routes (map ->handler (vals resources))))

(defmethod ->handler Collection
  [{:keys [method path route-handler]}]
  (make-route method (clout.core/route-compile path) route-handler))

(defmethod ->handler Member
  [{:keys [method path route-handler]}]
  (make-route method (clout.core/route-compile path) route-handler))

(defmethod ->handler Route
  [{:keys [method path route-handler]}]
  (make-route method (clout.core/route-compile path) route-handler))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- ^{:skip-wiki true} route-arguments
  "returns the list of route arguments in a route"
  [route]
  (let [args (re-seq #"/(:([^\/\.]+)|\*)" route)]
    (set (map #(keyword (or (nth % 2) (second %))) args))))

(defn- path-url [url route-args]
  (let [url (if (vector? url) ;;handle complex routes
              (first url)
              url)
        route-arg-names (route-arguments url)]
    (when-not (every? (set (keys route-args)) route-arg-names)
      (throwf "Missing route-args %s" (vec (filter #(not (contains? route-args %)) route-arg-names))))
    (reduce (fn [path [k v]]
              (if (= k :*)
                (string/replace path "*" (str v))
                (string/replace path (str k) (str v))))
            url
            route-args)))

(defn- query-url [uri params]
  (str (url uri params)))

(defn ->url
  "Returns the url belonging to the passed resource.
   Usage:
          (let [users (resources :users
                                 (resources :documents (coll :index :get (fn [req] \"User documents\"))))]
            (-> url [users :documents :index] {:route-params {:id 1}}))"
  [resource & [{:keys [route-params query-params]}]]
  (query-url
   (path-url (:path (if (vector? resource)
                      (reduce (fn [x1 x2] (x2 (:resources x1)))
                              resource)
                      resource))
             route-params) query-params))

(defmacro defresources
  "Creates a named resources value in the namespace and returns the handler"
  [res-name & args]
  `(do (def ~res-name (resources ~(keyword (str res-name)) ~@args))
       (->handler ~res-name)))

(defmacro routefn
  "A macro for wrapping a function definition with bindings used by compojure's destructuring request forms"
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defn- request-last-param-format
  [{uri :uri}]
  (keyword (second (string/split uri #"\."))))

(defmacro with-format
  "Used in case of extensions for the last parameter in the request's path
  Usage:
         (with-format request
           :json \"Json response\"
           \"No extension response\")"
  [request & case-body]
  `(case ~(request-last-param-format request)
     ~@case-body))
